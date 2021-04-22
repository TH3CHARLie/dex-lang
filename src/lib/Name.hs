-- Copyright 2021 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}



module Name (Name, RawName (..), Tag, ScopeP, Env, emptyScope, idEnv, envLookup,
             BinderP (..), BinderListP (..), getRawSourceName, fromRawSourceName,
             SourceNS, SourceName, pattern SourceBinder, Ext (..), (@>), (<.>), (<>>),
             extendEnvRename, withFresh, binderAnn, EnvFrag, (@@>), ScopeFragP,
             binderAsScopeFrag, emptyScopeFrag, extendScope, emptyEnvFrag,
             EmptyNest, NameSet, HasNames (..), liftNames, lowerNames,
             binderName, Abs (..), Nest (..), scopeLookup, toNest, fromNest,
             getRawName, NameHint, binderListBinders,
             RenameEnv, RenameEnvFrag) where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import Data.String
import qualified Data.Text as T
import GHC.Generics

import HigherKinded

type Tag = T.Text
data RawName = RawName Tag Int
               deriving (Show, Eq, Ord)

data Name n = UnsafeMakeName RawName
              deriving (Show, Eq, Ord)

data BinderP (ann :: * -> *) (n :: *) (l :: *) where
   UnsafeMakeBinder :: RawName -> ann n -> BinderP ann n l
   Ignore           :: ann n -> BinderP ann n n

instance (Show (ann n), Show n, Show l) => Show (BinderP ann n l)

class Ext n l | l -> n

data ScopeP e n = UnsafeMakeScope (M.Map RawName (e n))
                  deriving Show

data ScopeFragP (e :: * -> *) n n' = TodoMakeScopeFrag  deriving Show

emptyScopeFrag :: ScopeFragP e n n
emptyScopeFrag = undefined

data Env e i o = UnsafeMakeEnv (M.Map RawName (Either RawName (e o)))
                 deriving Show

data EnvFrag (e :: * -> *) i i' o = TodoMakeEnvFrag

emptyScope :: ScopeP e ()
emptyScope = undefined

idEnv :: Env e n a
idEnv = UnsafeMakeEnv M.empty

envLookup :: Env e i o -> Name i -> Either (Name o) (e o)
envLookup env name = undefined

scopeLookup :: ScopeP e n -> Name n -> e n
scopeLookup = undefined

infixl 5 <>>

(<>>) :: Env e i o -> EnvFrag e i i' o -> Env e i' o
(<>>) = undefined

infixr 7 @>

(@>) :: BinderP ann i i' -> e o -> EnvFrag e i i' o
(@>) = undefined

infixr 7 @@>

(@@>) :: Nest (BinderP ann) n l -> [e o] -> EnvFrag e i i' o
(@@>) = undefined


infixl 3 <.>

(<.>) :: EnvFrag e a b o -> EnvFrag e b c o -> EnvFrag e a c o
(<.>) = undefined


emptyEnvFrag :: EnvFrag e i i o
emptyEnvFrag = undefined

-- extendEnv :: BinderP ann n l -> e o -> Env e n o -> Env e l o
-- extendEnv = undefined

-- extendEnvMany :: BinderListP ann n l -> [e o] -> Env e n o -> Env e l o
-- extendEnvMany = undefined

extendEnvRename :: BinderP ann n l -> Name o -> Env e n o -> Env e l o
extendEnvRename = undefined

withFresh :: ann n -> ScopeP e n
          -> (forall l. Ext n l => BinderP ann n l -> a) -> a
withFresh _ _ _ = undefined

binderAsScopeFrag :: BinderP ann n l -> e l -> ScopeFragP e n l
binderAsScopeFrag = undefined

extendScope :: ScopeP e n -> ScopeFragP e n n' -> ScopeP e n'
extendScope = undefined

binderAnn :: BinderP ann n l -> ann n
binderAnn = undefined

binderName :: BinderP ann n l -> Name l
binderName = undefined

type NameSet n = S.Set (Name n)

class HasNames (e :: * -> *) where
  freeNames :: e n -> NameSet n

-- liftNames :: HasNames e => Ext n l => e n -> e l
liftNames :: Ext n l => e n -> e l
liftNames = undefined

-- TODO: use `Except` instead of `Maybe` to provide a more informative error
lowerNames :: ScopeFragP ann n l -> e l -> Maybe (e n)
-- lowerNames :: HasNames e => BinderP ann n l -> e l -> Maybe (e n)
lowerNames b expr = undefined

-- useful for printing etc
getRawName :: Name n -> RawName
getRawName (UnsafeMakeName name) = name

-- === unchecked source names don't obey any invariants ===

data SourceNS = SourceNS
type RawSourceName = Tag

type SourceName = Name SourceNS

pattern SourceBinder :: Name SourceNS -> ann SourceNS -> BinderP ann SourceNS SourceNS
pattern SourceBinder name ann <- ((\(UnsafeMakeBinder name ann) -> (SourceName name, ann)) -> (name, ann))
  where SourceBinder (UnsafeMakeName name) ann = UnsafeMakeBinder name ann

pattern SourceName :: RawName -> Name SourceNS
pattern SourceName name = UnsafeMakeName name

getRawSourceName :: Name SourceNS -> RawSourceName
getRawSourceName (UnsafeMakeName (RawName name 0)) = name
getRawSourceName (UnsafeMakeName (RawName _ _)) =
  error "nonzero counter for a source name shouldn't happen"

fromRawSourceName :: RawSourceName -> Name SourceNS
fromRawSourceName name = UnsafeMakeName (RawName name 0)

toNest :: [b SourceNS SourceNS] -> Nest b SourceNS SourceNS
toNest = undefined

fromNest :: Nest b SourceNS SourceNS -> [b SourceNS SourceNS]
fromNest = undefined

-- === convenience utilities ===

data Abs (binder :: * -> * -> *) (body :: * -> *) (n :: *) where
  Abs :: binder n l -> body l -> Abs binder body n

data Nest (binder :: * -> * -> * ) (n :: *) (l :: *) where
 Nest  :: binder n h -> Nest binder h i -> Nest binder n i
 Empty ::                                  Nest binder n n

type EmptyNest (binder :: * -> * -> *) = Abs (Nest binder) UnitH :: * -> *

instance Show (Abs ann body n)
instance Show (Nest ann result n)

type NameHint = RawName

data BinderListP ann n l = BinderListP (Nest (BinderP UnitH) n l) [ann n]

binderListBinders :: BinderListP ann n l -> Nest (BinderP UnitH) n l
binderListBinders = undefined

type RenameEnv     = Env     VoidH :: * -> * -> *
type RenameEnvFrag = EnvFrag VoidH :: * -> * -> * -> *


-- === instances ===

instance (HasNames e1, HasNames e2) => HasNames (PairH e1 e2) where
  freeNames = undefined

instance Eq (Abs b e n) where
  (==) = undefined

instance IsString RawName where
  fromString = undefined


-- === monadic interface ===




-- withFreshBuilder :: (forall l. Ext n l => Binder n l -> Builder l a) -> Builder n a

-- -- Let's figure out how to subst lambda with the monad
-- --  * doesn't have to be *completely* watertight




-- scoped :: Builder e n m => m (result n) -> Nest e result n









