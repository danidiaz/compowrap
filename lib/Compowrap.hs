{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Compowrap (Compowrappable, askWrapUnwrap, Compowrap (..)) where

import Data.Coerce
import Data.Functor.Compose
import Data.Kind
import Data.Proxy
import GHC.TypeLits (Natural)

type Compowrappable :: Natural -> Type -> Type -> Constraint
class
  Compowrappable n unwrapped wrapped
    | n unwrapped -> wrapped,
      n wrapped -> unwrapped
  where
  _wrap :: Proxy n -> unwrapped -> wrapped
  _unwrap :: Proxy n -> wrapped -> unwrapped

instance Compowrappable 2 (f (g tip)) (Compose f g tip) where
  _wrap Proxy = coerce
  _unwrap Proxy = coerce

instance (Functor f) => Compowrappable 3 (f (g (h tip))) (Compose f (Compose g h) tip) where
  _wrap Proxy = Compose . fmap Compose
  _unwrap Proxy = fmap getCompose . getCompose

instance (Functor f, Functor g) => Compowrappable 4 (f (g (h (i tip)))) ((Compose f (Compose g (Compose h i))) tip) where
  _wrap Proxy = Compose . fmap (Compose . fmap Compose)
  _unwrap Proxy = fmap (fmap getCompose . getCompose) . getCompose

-- | Formerly this type also had a phantom @ts@ parameter that was supposed to
-- help guide inference, but it seems not to be necessary?
type Compowrap :: Natural -> Type
data Compowrap n
  = WrapUnwrap
  { wrap :: forall {unwrapped} {wrapped}. (Compowrappable n unwrapped wrapped) => unwrapped -> wrapped,
    unwrap :: forall {unwrapped} {wrapped}. (Compowrappable n unwrapped wrapped) => wrapped -> unwrapped
  }

askWrapUnwrap ::
  forall n ->
  Compowrap n
askWrapUnwrap tn =
  WrapUnwrap
    { wrap = _wrap @_ (Proxy @tn),
      unwrap = _unwrap @_ (Proxy @tn)
    }
