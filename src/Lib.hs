{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Language.Haskell.TH

compose :: Q Exp
compose = [|\left right x -> left (right x)|]

someSplice :: Q [Dec]
someSplice = [d|y = 0|]
