{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}

module Compowrap (Compowrappable, askWrapUnwrap, Compowrap (..)) where

import Data.Coerce
import Data.Functor.Compose
import Data.Kind
import Data.List
import Data.Proxy
import GHC.TypeLits (Natural)

type Compowrappable :: Natural -> List (Type -> Type) -> Type -> Type -> Type -> Constraint
class
  Compowrappable n ts tip unwrapped wrapped
    | ts -> n,
      n unwrapped -> ts tip,
      n wrapped -> ts tip,
      ts tip -> unwrapped,
      ts tip -> wrapped
  where
  _wrap :: Proxy n -> unwrapped -> wrapped
  _unwrap :: Proxy n -> wrapped -> unwrapped

instance Compowrappable 2 [f, g] tip (f (g tip)) (Compose f g tip) where
  _wrap Proxy = coerce
  _unwrap Proxy = coerce

instance (Functor f) => Compowrappable 3 [f, g, h] tip (f (g (h tip))) (Compose f (Compose g h) tip) where
  _wrap Proxy = Compose . fmap Compose
  _unwrap Proxy = fmap getCompose . getCompose

instance (Functor f, Functor g) => Compowrappable 4 [f, g, h, i] tip (f (g (h (i tip)))) ((Compose f (Compose g (Compose h i))) tip) where
  _wrap Proxy = Compose . fmap (Compose . fmap Compose)
  _unwrap Proxy = fmap (fmap getCompose . getCompose) . getCompose

type Compowrap :: Natural -> List (Type -> Type) -> Type
data Compowrap n ts
  = WrapUnwrap
  { wrap :: forall tip unwrapped wrapped. (Compowrappable n ts tip unwrapped wrapped) => unwrapped -> wrapped,
    unwrap :: forall tip unwrapped wrapped. (Compowrappable n ts tip unwrapped wrapped) => wrapped -> unwrapped
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
