{-# LANGUAGE TemplateHaskell #-}

module PrimesTH where

import Language.Haskell.TH
import Primes (isPrime)

-- Run to examine AST instead of just runQ as usual:
-- runQ (fmap unType . examineCode $ primesUpTo' 10)
primesUpTo' :: Quote m => Integer -> Code m [Integer]
primesUpTo' n = go 2
  where
    go i
      | i > n     = [||[]||]
      | isPrime i = [||i : $$(go (i + 1))||]
      | otherwise = go (i + 1)
