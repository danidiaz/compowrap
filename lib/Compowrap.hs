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

type Compowrappable :: Natural -> (Type -> Type) -> Type -> Type -> Constraint
class
  Compowrappable n composed tip whole
    | n whole -> composed tip,
      n composed tip -> whole -- why is the n required in this FD?
  where
  _wrap :: Proxy n -> whole -> composed tip
  _unwrap :: Proxy n -> composed tip  -> whole

instance Compowrappable 2 (Compose f g) tip (f (g tip)) where
  _wrap Proxy = coerce
  _unwrap Proxy = coerce

instance (Functor f) => Compowrappable 3 (Compose f (Compose g h)) tip (f (g (h tip))) where
  _wrap Proxy = Compose . fmap Compose
  _unwrap Proxy = fmap getCompose . getCompose

instance (Functor f, Functor g) => Compowrappable 4 (Compose f (Compose g (Compose h i))) tip (f (g (h (i tip))))  where
  _wrap Proxy = Compose . fmap (Compose . fmap Compose)
  _unwrap Proxy = fmap (fmap getCompose . getCompose) . getCompose

-- | Formerly this type also had a phantom @ts@ parameter that was supposed to
-- help guide inference, but it seems not to be necessary?
type Compowrap :: Natural -> (Type -> Type) -> Type
data Compowrap n composed
  = WrapUnwrap
  { wrap :: forall tip {whole}. (Compowrappable n composed tip whole) => whole -> composed tip,
    unwrap :: forall tip {whole}. (Compowrappable n composed tip whole) => composed tip -> whole
  }

askWrapUnwrap ::
  forall {composed}.
  forall n ->
  Compowrap n composed
askWrapUnwrap tn =
  WrapUnwrap
    { wrap = _wrap @_ (Proxy @tn),
      unwrap = _unwrap @_ (Proxy @tn)
    }
