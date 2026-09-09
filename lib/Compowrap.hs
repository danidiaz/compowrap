{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Compowrap (Compowrap (..), askWrapUnwrap, someFunc) where

import Data.Functor.Compose
import Data.Functor.Identity
import Data.Proxy
import GHC.TypeLits (Natural)
import Data.List
import Data.Kind
import Data.Coerce

type family Compowrapped (ts :: List (Type -> Type)) :: Type -> Type where
  Compowrapped [f, g] = Compose f g 
  Compowrapped (f : rest) = Compose f (Compowrapped rest)

type family Compounwrapped (ts :: List (Type -> Type)) (tip :: Type) :: Type where
  Compounwrapped [f, g] tip = f (g tip)
  Compounwrapped (f : rest) tip = f (Compounwrapped rest tip)

class Compowrap (n :: Natural) (ts :: List (Type -> Type)) | ts -> n where
   
  askWrapUnwrap_ 
    :: 
      Proxy n ->
      forall tip . 
      (Compounwrapped ts tip -> Compowrapped ts tip,
      Compowrapped ts tip -> Compounwrapped ts tip)

instance Compowrap 2 [f,g] where
  askWrapUnwrap_ Proxy = (\u -> coerce u , \w -> coerce w)

instance Functor f => Compowrap 3 [f,g,h] where
  askWrapUnwrap_ Proxy = (Compose . fmap Compose, fmap getCompose . getCompose)

instance (Functor f, Functor g) => Compowrap 4 [f,g,h,i] where
  askWrapUnwrap_ Proxy = (Compose . fmap (Compose . fmap Compose), fmap (fmap getCompose . getCompose) . getCompose)

askWrapUnwrap 
  :: 
     forall {ts} {tip}. 
     forall n -> 
     (Compowrap n ts) =>
      (Compounwrapped ts tip -> Compowrapped ts tip,
      Compowrapped ts tip -> Compounwrapped ts tip)
askWrapUnwrap tn = askWrapUnwrap_ @_ @ts (Proxy @tn)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
