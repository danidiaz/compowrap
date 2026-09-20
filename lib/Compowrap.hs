{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}

module Compowrap (Compowrappable, Compowrapped, askWrapUnwrap, Compowrap (..)) where

import Data.Coerce
import Data.Functor.Compose
import Data.Kind
import Data.List
import Data.Proxy
import GHC.TypeLits (Natural)

type Compowrapped :: List (Type -> Type) -> Type -> Type
type family Compowrapped ts :: Type -> Type where
  Compowrapped [f, g] = Compose f g
  Compowrapped (f : rest) = Compose f (Compowrapped rest)

type Compowrappable :: Natural -> List (Type -> Type) -> Type -> Type -> Constraint
class Compowrappable n ts tip whole | ts -> n, n whole -> ts tip, ts tip -> whole where
  _wrap :: Proxy n -> whole -> Compowrapped ts tip
  _unwrap :: Proxy n -> Compowrapped ts tip -> whole

instance Compowrappable 2 [f, g] tip (f (g tip)) where
  _wrap Proxy = coerce
  _unwrap Proxy = coerce

instance (Functor f) => Compowrappable 3 [f, g, h] tip (f (g (h tip))) where
  _wrap Proxy = Compose . fmap Compose
  _unwrap Proxy = fmap getCompose . getCompose

instance (Functor f, Functor g) => Compowrappable 4 [f, g, h, i] tip (f (g (h (i tip)))) where
  _wrap Proxy = Compose . fmap (Compose . fmap Compose)
  _unwrap Proxy = fmap (fmap getCompose . getCompose) . getCompose

type Compowrap :: Natural -> List (Type -> Type) -> Type
data Compowrap n ts
  = WrapUnwrap
  { wrap :: forall tip whole. (Compowrappable n ts tip whole) => whole -> Compowrapped ts tip,
    unwrap :: forall tip whole. (Compowrappable n ts tip whole) => Compowrapped ts tip -> whole
  }

askWrapUnwrap ::
  forall {ts}.
  forall n ->
  Compowrap n ts
askWrapUnwrap tn =
  WrapUnwrap
    { wrap = _wrap @_ @ts (Proxy @tn),
      unwrap = _unwrap @_ @ts (Proxy @tn)
    }
