module Primes where

isPrime :: Integer -> Bool
isPrime n
  | n <= 1    = False
  | n == 2    = True
  | even n    = False  -- No even number except for 2 is prime
  | otherwise = go 3
  where
    go i
      | i >= n         = True  -- We saw all smaller numbers and no divisors, so it's prime
      | n `mod` i == 0 = False
      | otherwise      = go (i + 2)  -- Iterate through the odd numbers

primesUpTo :: Integer -> [Integer]
primesUpTo n = go 2
  where
    go i
      | i > n     = []
      | isPrime i = i : go (i + 1)
      | otherwise = go (i + 1)
