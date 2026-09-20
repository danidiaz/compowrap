module Main (main) where

import Compowrap
import Data.Function ((&))

stuff, stuff' :: Either Int (Either Int (Either Int Bool))
stuff = Right (Right (Right False))
stuff' =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap 3
   in stuff
        & wrap
        & fmap not
        & unwrap

-- removing this signature causes a compilation error. But should it work without it?
stuff'' :: Either Int (Either Int (Either Int String))
stuff'' =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap 3
   in unwrap $ show <$> ((&&) <$> wrap stuff <*> wrap stuff)

main :: IO ()
main = do
    putStrLn $ show stuff
    putStrLn $ show stuff'
    putStrLn $ show stuff''
