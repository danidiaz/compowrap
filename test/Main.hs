module Main (main) where

import Compowrap
import Data.Function ((&))

stuff, stuff':: Either Int (Either Int (Either Int Bool))
stuff = Right (Right (Right False))
stuff' =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap 3
   in stuff
        & wrap
        & fmap not
        & unwrap

-- works even without signature.
-- I had to add the 'wrapped' type variable to 'Compowrappable' for this to work without signature.
-- stuff'' :: Either Int (Either Int (Either Int String))
stuff'' =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap 3
   in unwrap $ (\a b -> show $ a && b) <$> wrap stuff <*> wrap stuff

stuff''' =
  nliftA2 3 (\a b -> show $ a && b) stuff stuff

atuff, atuff' :: Either String (Either Float (Either Int Bool))
atuff = Right (Right (Right False))
atuff' =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap 3
   in atuff
        & wrap
        & fmap not
        & unwrap

atuff'' =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap 3
   in unwrap $ (\a b -> show $ a && b) <$> wrap atuff <*> wrap atuff

atuff''' =
  nliftA2 3 (\a b -> show $ a && b) atuff atuff'

main :: IO ()
main = do
  putStrLn $ show stuff
  putStrLn $ show stuff'
  putStrLn $ show stuff''
  putStrLn $ show atuff
  putStrLn $ show atuff'
  putStrLn $ show atuff''
