{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FunctionalDependencies #-}
module Compowrap (someFunc) where
import GHC.TypeLits (Natural)
import Data.Functor.Compose
import Data.Proxy


class Compowrap (n :: Natural) outer inner inner_ tip | n inner -> inner_ tip where
    askWrapUnwrap :: Proxy n -> outer inner  -> Compose outer inner_ tip

someFunc :: IO ()
someFunc = putStrLn "someFunc"
