{-# LANGUAGE TemplateHaskell #-}

module TuplesTH where

import Control.Monad (unless)
import Data.Traversable (for)
import Language.Haskell.TH

generateTupleClass :: Int -> Q [Dec]
generateTupleClass size = do
  unless (size > 0) $
    fail $ "Non-positive size: " ++ size'
  pure [cDecl]
  where
    size' = show size
    className = mkName ("Tuple" ++ size')
    methodName = mkName ('_' : size')

    t = mkName "t"
    r = mkName "r"

    -- class TupleX t r | t -> r where
    cDecl = ClassD [] className [PlainTV t (), PlainTV r ()] [FunDep [t] [r]] [mDecl]

    --   _X :: t -> r
    mDecl = SigD methodName (AppT (AppT ArrowT (VarT t)) (VarT r))
