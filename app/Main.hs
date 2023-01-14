{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

import Language.Haskell.TH
import Lib
import TuplesTH

v :: Int
v = 42

$someSplice

z :: String
z = show v

main :: IO ()
main = do
  let res :: Integer = $compose (*3) (+2) 1
  print res

$(generateTupleBoilerplate 62)
