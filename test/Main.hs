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

-- works even with signature.
-- I had to add the 'wrapped' type variable to 'Compowrappable' for this to work.
-- stuff'' :: Either Int (Either Int (Either Int String))
stuff'' =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap 3
   in unwrap $ show <$> ((&&) <$> wrap stuff <*> wrap stuff)

atuff, atuff' :: Either String (Either Float (Either Int Bool))
atuff = Right (Right (Right False))
atuff' =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap 3
   in atuff
        & wrap
        & fmap not
        & unwrap

-- works even with signature.
-- I had to add the 'wrapped' type variable to 'Compowrappable' for this to work.
-- astuff'' :: Either Int (Either Int (Either Int String))
atuff'' =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap 3
   in unwrap $ show <$> ((&&) <$> wrap atuff <*> wrap atuff)

main :: IO ()
main = do
  putStrLn $ show stuff
  putStrLn $ show stuff'
  putStrLn $ show stuff''
  putStrLn $ show atuff
  putStrLn $ show atuff'
  putStrLn $ show atuff''
