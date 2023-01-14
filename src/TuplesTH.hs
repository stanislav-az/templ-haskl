{-# LANGUAGE TemplateHaskell #-}

module TuplesTH where

import Control.Monad (unless)
import Data.Traversable (for)
import Language.Haskell.TH

generateTupleClass :: Int -> Q [Dec]
generateTupleClass size = do
  unless (size > 0) $
    fail $
      "Non-positive size: " ++ size'
  pure [cDecl]
  where
    size' = show size
    className = mkName ("Tuple" ++ size')
    getterName = mkName ('_' : size')
    mapperName = mkName ("map" ++ size')

    t = mkName "t"
    r = mkName "r"

    -- class TupleX t r | t -> r where
    cDecl = ClassD [] className [PlainTV t (), PlainTV r ()] [FunDep [t] [r]] [getter, mapper]

    -- _X :: t -> r
    getter = SigD getterName (AppT (AppT ArrowT (VarT t)) (VarT r))

    -- mapX :: (r -> r) -> t -> t
    mapper =
      let innerArrow = AppT (AppT ArrowT (VarT r)) (VarT r)
       in SigD mapperName (AppT (AppT ArrowT innerArrow) (AppT (AppT ArrowT (VarT t)) (VarT t)))

generateTupleInstance :: Int -> Int -> Q [Dec]
generateTupleInstance element size = do
  unless (size > 0) $
    fail $
      "Non-positive size: " ++ element'
  unless (size >= element) $
    fail $
      "Can't extract element " ++ element' ++ " of " ++ size' ++ "-tuple"
  pure [iDecl]
  where
    element' = show element
    size' = show size
    className = mkName ("Tuple" ++ element')
    getterName = mkName ('_' : element')
    mapperName = mkName ("map" ++ element')

    x = mkName "x"

    vars = [mkName ('t' : show n) | n <- [1 .. size]]

    signature = foldl (\acc var -> AppT acc (VarT var)) (TupleT size) vars

    -- instance TupleX (t1, ..., tX, ...) tX where
    iDecl = InstanceD Nothing [] (AppT (AppT (ConT className) signature) (VarT $ mkName ('t' : element'))) [getter, mapper]

    -- _X (_, _, ..., x, ...) = x
    getter = FunD getterName [Clause [TupP $ replicate (element - 1) WildP ++ [VarP x] ++ replicate (size - element) WildP] (NormalB $ VarE x) []]

    -- mapX f (t1, t2, ..., tX, ..., tN) = (t1, t2, ..., f tX, ..., tN)
    mapper =
      let f = mkName "f"
          pats = map VarP vars
          mkVarE i
            | i == element = AppE (VarE f) (VarE $ mkName ('t' : show i))
            | otherwise = VarE $ mkName ('t' : show i)
       in FunD mapperName [Clause [VarP f, TupP pats] (NormalB $ TupE $ map (Just . mkVarE) [1 .. size]) []]

generateTupleBoilerplate :: Int -> Q [Dec]
generateTupleBoilerplate size =
  concatFor [1 .. size] $ \classDeclIndex -> do
    cDecl <- generateTupleClass classDeclIndex
    iDecls <- for [1 .. classDeclIndex] $ \instanceDeclIndex ->
      generateTupleInstance instanceDeclIndex classDeclIndex

    pure $ concat (cDecl : iDecls)
  where
    concatFor xs = fmap concat . for xs
