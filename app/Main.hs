{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
  let res :: Integer = $compose (* 3) (+ 2) 1
  print res

$(generateTupleBoilerplate 10)
-- MAX tuple size = 62
