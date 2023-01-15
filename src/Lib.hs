{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Language.Haskell.TH

compose :: Q Exp
compose = [|\left right x -> left (right x)|]

someSplice :: Q [Dec]
someSplice = [d|y = 0|]

myFunc :: Q Exp
myFunc = do
  x <- newName "x" -- generate a unique variable name, we'll cover names later
  return $ LamE    -- lambda expression
    [VarP x]       -- pattern matching on 'x'
    (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1))))
    -- here we have an infix expression: we apply (+) to 'x' and integer
    -- literal 1

myFuncTyped :: (Quote m) => Code m (Integer -> Integer)
myFuncTyped = [|| \x -> x + 1 ||]
