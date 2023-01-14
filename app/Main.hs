{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib

main :: IO ()
main = do
  let res :: Integer = $compose (*3) (+2) 1
  print res
