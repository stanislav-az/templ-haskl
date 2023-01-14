{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Language.Haskell.TH
import Lib
import TuplesTH

x :: Int
x = 42

$someSplice

z :: String
z = show x

main :: IO ()
main = do
  let res :: Integer = $compose (*3) (+2) 1
  print res

$(generateTupleClass 3)
