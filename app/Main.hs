{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Language.Haskell.TH
import Lib

x :: Int
x = 42

$someSplice

z :: String
z = show x

main :: IO ()
main = do
  let res :: Integer = $compose (*3) (+2) 1
  print res
