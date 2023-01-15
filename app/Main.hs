{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-} -- dumps generated th to stderr
{-# OPTIONS_GHC -ddump-to-file #-} -- dumps generated th to .stack-work/somewhere/file.dump-splices

import Language.Haskell.TH
import Lib
import TuplesTH
import CountableTH
import Data.Word ( Word8 )
import CountableTH
import Data.Proxy

v :: Int
v = 42

$someSplice

z :: String
z = show v

main :: IO ()
main = do
  let res :: Integer = $compose (* 3) (+ 2) 1
  print res

-- MAX tuple size = 62
-- Can be rewritten as:
-- generateTupleBoilerplate 10
$(generateTupleBoilerplate 10)

deriveCountable ''Bool
deriveCountable ''Word8
deriveCountable ''Char

data Foo
  = Foo Bool Bool
  | Bar Word8 Bool

deriveCountable ''Foo
