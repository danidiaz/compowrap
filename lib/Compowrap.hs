{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Compowrap (Compowrappable, Compowrapped, askWrapUnwrap, Compowrap(..)) where

import Data.Functor.Compose
import Data.Proxy
import GHC.TypeLits (Natural)
import Data.List
import Data.Kind
import Data.Coerce

type Compowrapped :: List (Type -> Type) -> Type -> Type
type family Compowrapped ts :: Type -> Type where
  Compowrapped [f, g] = Compose f g 
  Compowrapped (f : rest) = Compose f (Compowrapped rest)

type Compowrappable :: Natural -> List (Type -> Type) -> Type -> Type -> Constraint
class Compowrappable n ts tip whole | ts -> n, n whole -> ts tip, ts tip -> whole where
   
  askWrapUnwrap_ 
    :: 
      Proxy n ->
      (whole -> Compowrapped ts tip,
      Compowrapped ts tip -> whole)

instance Compowrappable 2 [f,g] tip (f (g tip)) where
  askWrapUnwrap_ Proxy = (\u -> coerce u , \w -> coerce w)

instance Functor f => Compowrappable 3 [f,g,h] tip (f (g (h tip))) where
  askWrapUnwrap_ Proxy = (Compose . fmap Compose, fmap getCompose . getCompose)

instance (Functor f, Functor g) => Compowrappable 4 [f,g,h,i] tip (f (g (h (i tip)))) where
  askWrapUnwrap_ Proxy = (Compose . fmap (Compose . fmap Compose), fmap (fmap getCompose . getCompose) . getCompose)

type Compowrap :: Natural -> List (Type -> Type) -> Type
data Compowrap n ts where
  WrapUnwrap :: 
    { wrap :: forall tip whole . Compowrappable n ts tip whole => whole -> Compowrapped ts tip,
      unwrap :: forall tip whole . Compowrappable n ts tip whole => Compowrapped ts tip -> whole
     } -> Compowrap n ts

askWrapUnwrap 
  :: 
     forall {ts}. 
     forall n -> 
     Compowrap n ts
askWrapUnwrap tn = 
  WrapUnwrap
    { wrap = 
        let (wrap, _) = askWrapUnwrap_ @_ @ts (Proxy @tn)
         in wrap
      ,
      unwrap =
        let (_, unwrap) = askWrapUnwrap_ @_ @ts (Proxy @tn)
         in unwrap
     }
