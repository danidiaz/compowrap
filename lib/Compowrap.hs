{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Compowrap
  ( Compowrappable,
    askWrapUnwrap,
    Compowrap (..),
    npure,
    nliftA,
    nfmap,
    nliftA2,
    nliftA3,
  )
where

import Control.Applicative
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
  _unwrap :: Proxy n -> composed tip -> whole

instance Compowrappable 2 (Compose f g) tip (f (g tip)) where
  _wrap Proxy = coerce
  _unwrap Proxy = coerce

instance (Functor f) => Compowrappable 3 (Compose f (Compose g h)) tip (f (g (h tip))) where
  _wrap Proxy = Compose . fmap Compose
  _unwrap Proxy = fmap getCompose . getCompose

instance (Functor f, Functor g) => Compowrappable 4 (Compose f (Compose g (Compose h i))) tip (f (g (h (i tip)))) where
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
askWrapUnwrap n =
  WrapUnwrap
    { wrap = _wrap @_ (Proxy @n),
      unwrap = _unwrap @_ (Proxy @n)
    }

nfmap ::
  forall {composed} a b {whole_a} {whole_b}.
  (Functor composed) =>
  forall (n :: Natural) ->
  ( Compowrappable n composed a whole_a,
    Compowrappable n composed b whole_b
  ) =>
  (a -> b) ->
  whole_a ->
  whole_b
nfmap n f whole_a =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap n
   in unwrap (fmap f (wrap whole_a))

npure ::
  forall {composed} a {whole}.
  (Applicative composed) =>
  forall (n :: Natural) ->
  (Compowrappable n composed a whole) =>
  a ->
  whole
npure n a = _unwrap @_ (Proxy @n) (pure a)

nliftA ::
  forall {composed} a b {whole_a} {whole_b}.
  (Applicative composed) =>
  forall (n :: Natural) ->
  ( Compowrappable n composed a whole_a,
    Compowrappable n composed b whole_b
  ) =>
  (a -> b) ->
  whole_a ->
  whole_b
nliftA n f whole_a =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap n
   in unwrap (liftA f (wrap whole_a))

nliftA2 ::
  forall {composed} a b c {whole_a} {whole_b} {whole_c}.
  (Applicative composed) =>
  forall (n :: Natural) ->
  ( Compowrappable n composed a whole_a,
    Compowrappable n composed b whole_b,
    Compowrappable n composed c whole_c
  ) =>
  (a -> b -> c) ->
  whole_a ->
  whole_b ->
  whole_c
nliftA2 n f whole_a whole_b =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap n
   in unwrap (liftA2 f (wrap whole_a) (wrap whole_b))

nliftA3 ::
  forall {composed} a b c d {whole_a} {whole_b} {whole_c} {whole_d}.
  (Applicative composed) =>
  forall (n :: Natural) ->
  ( Compowrappable n composed a whole_a,
    Compowrappable n composed b whole_b,
    Compowrappable n composed c whole_c,
    Compowrappable n composed d whole_d
  ) =>
  (a -> b -> c -> d) ->
  whole_a ->
  whole_b ->
  whole_c ->
  whole_d
nliftA3 n f whole_a whole_b whole_c =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap n
   in unwrap (liftA3 f (wrap whole_a) (wrap whole_b) (wrap whole_c))