module Main (main) where

import Compowrap
import Data.Function ((&))

stuff, stuff', stuff'' :: Either Int (Either Int (Either Int Bool))
stuff = Right (Right (Right False))

stuff' =  
    let WrapUnwrap {wrap, unwrap} = askWrapUnwrap 3
     in stuff
        & wrap
        & fmap not 
        & unwrap

stuff'' =  
    let WrapUnwrap {wrap, unwrap} = askWrapUnwrap 3
     in unwrap $ and <$> wrap stuff <*> wrap stuff

main :: IO ()
main = putStrLn "Test suite not yet implemented."
