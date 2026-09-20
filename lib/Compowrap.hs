{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Compowrap (Compowrappable (..)) where

import Data.Functor.Compose
import Data.Proxy
import GHC.TypeLits (Natural)
import Data.List
import Data.Kind
import Data.Coerce

-- class Compowrap' 
--         (n :: Natural) 
--         (ts :: List (Type -> Type)) 
--         tip 
--         everything
--         | ts -> n, n everything -> ts tip, n ts tip -> everything where
--     type Compowrap n ts tip everything
--   askWrapUnwrap_ 
--     :: 
--       Proxy n ->
--       forall tip . 
--       (Compounwrapped ts tip -> Compowrap ts tip,
--       Compowrap ts tip -> Compounwrapped ts tip)

type family Compowrap (ts :: List (Type -> Type)) :: Type -> Type where
  Compowrap [f, g] = Compose f g 
  Compowrap (f : rest) = Compose f (Compowrap rest)

-- type family Compounwrapped (ts :: List (Type -> Type)) (tip :: Type) :: Type where
--   Compounwrapped [f, g] tip = f (g tip)
--   Compounwrapped (f : rest) tip = f (Compounwrapped rest tip)

type Compowrappable :: Natural -> List (Type -> Type) -> Type -> Type -> Constraint
class Compowrappable n ts tip whole | ts -> n, n whole -> ts tip, ts tip -> whole where
   
  askWrapUnwrap_ 
    :: 
      Proxy n ->
      (whole -> Compowrap ts tip,
      Compowrap ts tip -> whole)

instance Compowrappable 2 [f,g] tip (f (g tip)) where
  askWrapUnwrap_ Proxy = (\u -> coerce u , \w -> coerce w)

instance Functor f => Compowrappable 3 [f,g,h] tip (f (g (h tip))) where
  askWrapUnwrap_ Proxy = (Compose . fmap Compose, fmap getCompose . getCompose)

instance (Functor f, Functor g) => Compowrappable 4 [f,g,h,i] tip (f (g (h (i tip)))) where
  askWrapUnwrap_ Proxy = (Compose . fmap (Compose . fmap Compose), fmap (fmap getCompose . getCompose) . getCompose)

-- askWrapUnwrap 
--   :: 
--      forall {ts} {tip}. 
--      forall n -> 
--      (Compowrappable n ts) =>
--       (Compounwrapped ts tip -> Compowrap ts tip,
--       Compowrap ts tip -> Compounwrapped ts tip)
-- askWrapUnwrap tn = askWrapUnwrap_ @_ @ts (Proxy @tn)
