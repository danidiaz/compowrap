module Main (main) where

import Compowrap
import Data.Function ((&))

stuff, stuff' :: Either Int (Either Int (Either Int Bool))
stuff = Right (Right (Right False))

stuff' =  
    let (wrap, unwrap) = askWrapUnwrap 3
     in stuff
        & wrap
        & fmap not 
        & unwrap

main :: IO ()
main = putStrLn "Test suite not yet implemented."
