# Copyright 2020 Google LLC
#
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file or at
# https://developers.google.com/open-source/licenses/bsd

from weakref import WeakKeyDictionary
from functools import partial
from itertools import count
import ctypes
import numpy as np

import jax
from jax.lib import xla_client as xc
from jax.interpreters import xla
from jax.interpreters import batching

from .. import Atom, eval
from ..native_function import IdxRepTy, ScalarType, RectContArrayType
from .. import api

def primitive(f):
  if not isinstance(f, Atom):
    raise TypeError("DexPrimitive expects a function atom as an argument")
  return partial(dex_call_p.bind, func_atom=f)

compiler_cache = WeakKeyDictionary()
def get_compiled(func_atom):
  compiled = compiler_cache.get(func_atom, None)
  if compiled is None:
    compiled = compiler_cache[func_atom] = func_atom.compile()
  return compiled


dex_call_p = jax.core.Primitive('dex_call')

@dex_call_p.def_impl
def dex_call_impl(*args, func_atom):
  return get_compiled(func_atom)(*args)

# === abstract evaluation / shape inference ===

def dex_call_abstract_eval_with_shape(*args, func_atom):
  # TODO: Make it possible to get the signature without compiling the function
  native_func = get_compiled(func_atom)
  arg_sig = native_func.explicit_argument_signature
  res_sig = native_func.result_signature
  if len(args) != len(arg_sig):
    raise RuntimeError(f"Dex function expects {len(arg_sig)} arguments, but was given {len(args)}")
  if not all(isinstance(arg, jax.core.ShapedArray) for arg in args):
    raise RuntimeError("Cannot perform evaluation of Dex functions without known shapes")
  # Check arguments and infer shape parameters
  shape_vars = {}
  for i, (arg, b) in enumerate(zip(args, arg_sig)):
    expected_dtype = np.dtype(b.type.ctype)
    if arg.dtype != expected_dtype:
      raise RuntimeError(f"dtype mismatch in arg {i}: expected {expected_dtype}, got {arg.dtype}")
    if isinstance(b.type, ScalarType):
      expected_shape = ()
    elif isinstance(b.type, RectContArrayType):
      expected_shape = b.type.shape
    else:
      raise AssertionError("Unhandled case!")
    if len(arg.shape) != len(expected_shape):
      raise RuntimeError(f"rank mismatch in arg {i}: expected {len(expected_shape)}, got {len(arg.shape)}")
    inferred_shape = tuple(
      size if isinstance(size, int) else shape_vars.setdefault(size, real_size)
      for size, real_size in zip(expected_shape, arg.shape))
    if arg.shape != inferred_shape:
      raise RuntimeError(f"shape mismatch in arg {i}: expected {inferred_shape}, got {arg.shape}")
  # Infer result types
  result_avals = []
  for b in res_sig:
    dtype = np.dtype(b.type.ctype)
    if isinstance(b.type, ScalarType):
      shape = ()
    elif isinstance(b.type, RectContArrayType):
      shape = tuple(shape_vars.get(size, size) for size in b.type.shape)
    result_avals.append(jax.core.ShapedArray(shape, dtype))
  assert len(result_avals) == 1  # TODO: Make dex_call a multiple_results primitive
  return result_avals[0], shape_vars

@dex_call_p.def_abstract_eval
def dex_call_abstract_eval(*args, **kwargs):
  return dex_call_abstract_eval_with_shape(*args, **kwargs)[0]

# === xla translation ===

PyCapsule_Destructor = ctypes.CFUNCTYPE(None, ctypes.py_object)
PyCapsule_New = ctypes.pythonapi.PyCapsule_New
PyCapsule_New.restype = ctypes.py_object
PyCapsule_New.argtypes = (ctypes.c_void_p, ctypes.c_char_p, PyCapsule_Destructor)

def make_custom_call_target(func_ptr):
  return PyCapsule_New(func_ptr, b"xla._CUSTOM_CALL_TARGET", PyCapsule_Destructor(0))

# TODO: Better lifetime management. func_atoms will be quite often created on the fly
#       at trace time when different transforms are applied, and I'm pretty sure that
#       the XLA executables outlive jaxprs formed by tracing.
custom_call_id = count()
custom_call_cache = {}
def dex_call_cpu_translation(b, *args, func_atom):
  xla_shapes = list(map(b.get_shape, args))
  result_aval, shape_vars = dex_call_abstract_eval_with_shape(
      *(jax.core.ShapedArray(xshape.dimensions(), xshape.numpy_dtype())
        for xshape in xla_shapes),
      func_atom=func_atom)
  result_xshape = xc.Shape.array_shape(result_aval.dtype, result_aval.shape)

  custom_call = custom_call_cache.get(func_atom, None)
  native = get_compiled(func_atom)
  if custom_call is None:
    assert len(args) == len(native.explicit_argument_signature)
    assert 1 == len(native.result_signature)
    custom_call_ctype = ctypes.CFUNCTYPE(None,
                                         ctypes.c_void_p,
                                         ctypes.POINTER(ctypes.c_void_p * len(args)))
    @custom_call_ctype
    def trampoline(result_ptr, arg_ptr_array):
      name_to_cval = {name: IdxRepTy(value) for name, value in shape_vars.items()}
      for binder, ptr in zip(native.explicit_argument_signature, arg_ptr_array.contents):
        if isinstance(binder.type, ScalarType):
          cval = ctypes.cast(ptr, ctypes.POINTER(binder.type.arg_ctype)).contents
        elif isinstance(binder.type, RectContArrayType):
          cval = ctypes.cast(ptr, binder.type.arg_ctype)
        else:
          raise AssertionError("Unexpected binder type")
        name_to_cval[binder.name] = cval
      result_binder = native.result_signature[0]
      name_to_cval[result_binder.name] = ctypes.cast(result_ptr, result_binder.type.ref_ctype)
      native.callable(*(name_to_cval[name] for name in native.ccall_signature))

    trampoline_addr = ctypes.c_void_p.from_param(trampoline)
    custom_call_name = f"dex_custom_call{next(custom_call_id)}".encode('ascii')
    xc.register_custom_call_target(custom_call_name,
                                   make_custom_call_target(trampoline_addr))
    custom_call_cache[func_atom] = (custom_call_name, trampoline)
    # TODO: Unregister custom calls at some point?
  else:
    custom_call_name, *_ = custom_call
  return xc.ops.CustomCall(b, custom_call_name, operands=args, shape=result_xshape)

jax.interpreters.xla.backend_specific_translations['cpu'][dex_call_p] = dex_call_cpu_translation


# === batching ===

def dex_call_batched(batched_args, batched_dims, func_atom):
  """Batching function for dex primitives.

  Args:
    batched_args: The possibly-batched arguments.
    batched_dims: A sequence of the same length as `batched_args`, where each
      entry indicates the batching axis of the corresponding entry to `args`,
      or None if that argument should not be batched. Not all entries can be
      None.

  Returns:
    2-tuple containing the result of the batched function, and the result axis
    which was batched, which is always zero.
  """
  # Move axes so that we only have to deal with the zero axis being batched.
  uniform_batched_args = [
      batching.moveaxis(arg, bd, 0) if bd is not batching.not_mapped else arg
      for arg, bd in zip(batched_args, batched_dims)
  ]

  # This assumes not all entries in batched_dims are None.
  batch_size = next(
      arg.shape[0] for arg, bd in zip(uniform_batched_args, batched_dims)
      if bd is not batching.not_mapped)

  # Add the current function atom as a variable in the context, so that we can
  # use it to apply batching.
  env = api.insert(func_atom.module, api.as_cstr("jax_batched_func"), func_atom)

  # Only index into the arguments which are batched. `i` is the index used for
  # the Dex for loop constructor.
  batched_fn_params = [
      f"x{param_idx}" if dim is batching.not_mapped else f"x{param_idx}.i"
      for param_idx, dim in enumerate(batched_dims)
  ]

  # This is the actual batching expression
  batched_fn = eval(
      r"\ " + " ".join(f"x{i}" for i in range(len(batched_args))) + ". "
      + f"for i:(Fin {batch_size}). jax_batched_func "
      + " ".join(batched_fn_params), module=env)

  return primitive(batched_fn)(*uniform_batched_args), 0


batching.primitive_batchers[dex_call_p] = dex_call_batched


# === jvp / linearize  ===

def dex_call_jvp(arg_values, arg_tangents, func_atom):
  """Evaluates the function output at arg_values, and the linearized function
  (linearized about arg_values) at arg_tangents.

  Args:
    arg_values: A tuple of arguments.
    arg_tangents: A tuple with the tangents of the arguments. The tuple has the
      same length as the arg_values. Some of the tangents may also be the
      special value ad.Zero to specify a zero tangent.
    func_atom: Function atom to linearize. The result type of this function
      atom must be a single array type.

  Returns:
     A pair of the primal output and the tangent.
  """
  assert len(func_atom.compile().result_signature) == 1
  num_args = len(arg_values)

  # Helper functions to build strings of primal and tangent inputs.
  def arg_string(prefix):
    return " ".join(f"{prefix}{i}" for i in range(num_args))

  def tuple_string(prefix):
    return "(" + ", ".join(f"{prefix}{i}" for i in range(num_args)) + ")"

  # Add the current function atom as a variable in the context, so that we can
  # use it to apply batching.
  env = api.insert(func_atom.module, api.as_cstr("jax_func"), func_atom)

  # `linearize` only seems to work properly for functions which take a single
  # input argument, so we uncurry `func_atom` to make it into this form. The
  # evaluated string for three function arguments should look like:
  # ```
  # \ (x0, x1, x2). jax_func x0 x1 x2
  # ```
  uncurried = eval(
      f"\\ {tuple_string('x')}. jax_func {arg_string('x')}",
      module=env)
  old_env, env = env, api.insert(env, api.as_cstr("jax_func_uncurried"),
                                 uncurried)
  # Destroy the intermediate context.
  api.destroyContext(old_env)

  # We create separate primitives for the primal and tangent evaluations, since
  # we only want to apply tranposition to the tangent evaluation function.
  #
  # Here we write out the tangent evaluation expression in pointful style.
  # The evaluated string for three function arguments should look like:
  # ```
  # \ x0 x1 x2 u0 u1 u2.
  #   linearized = linearize jax_func_uncurried (x0, x1, x2)
  #   snd linearized (u0 u1 u2)
  # ```
  evaluate_linearized = eval(
      f"\\ {arg_string('x')} {arg_string('u')}." +
      f"\n  linearized = linearize jax_func_uncurried {tuple_string('x')}" +
      f"\n  snd linearized {tuple_string('u')}",
      module=env)

  # Materialize jax.ad.Zero values into actual arrays of zeros.
  # TODO: Make the handling of Zeros more efficient by omitting them from the
  # linearize expression. This would avoid having to create these zero
  # arguments, although it might make constructing the transpose expression
  # more fiddly.
  tangents_no_zeros = [
      jax.lax.zeros_like_array(arg) if type(tan) is jax.ad.Zero else tan
      for arg, tan in zip(arg_values, arg_tangents)
  ]

  return (
      primitive(func_atom)(*arg_values),
      primitive(evaluate_linearized)(*arg_values, *tangents_no_zeros),
  )

jax.interpreters.ad.primitive_jvps[dex_call_p] = dex_call_jvp

# TODO
# jax.interpreters.ad.primitive_transposes[self.primitive] = ...
