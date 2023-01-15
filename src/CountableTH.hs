{-# LANGUAGE TemplateHaskell #-}

module CountableTH where

import Language.Haskell.TH
import Data.Proxy

class Countable a where
  count :: Proxy a -> Integer

deriveCountableSimple :: Name -> Q [Dec]
deriveCountableSimple name = [d|
  instance Countable $a where
    count Proxy = fromIntegral $
      1 + fromEnum (maxBound :: $a) - fromEnum (minBound :: $a)
  |]
  where
    a = conT name

deriveCountableComposite :: Name -> Q [Dec]
deriveCountableComposite name = do
  TyConI (DataD _ _ _ _ cons' _) <- reify name
  [d|
     instance Countable $(conT name) where
       count Proxy = $(foldr addE [| 0 |] $ f <$> cons')
   |]
  where
    f (NormalC _ ts) = handleCon (snd <$> ts)
    f (RecC    _ ts) = handleCon (thd <$> ts)
    f _              = fail "unsupported data type"
    handleCon ts = foldr mulE [| 1 |] (countTypeE <$> ts)
    countTypeE t = [| count (Proxy :: Proxy $(return t)) |]
    addE x y     = [| $x + $y |]
    mulE x y     = [| $x * $y |]
    thd (_,_,x)  = x

deriveCountable :: Name -> Q [Dec]
deriveCountable name = do
  let ts = [ConT name]
  hasEnum    <- isInstance ''Enum    ts
  hasBounded <- isInstance ''Bounded ts
  if hasEnum && hasBounded
    then deriveCountableSimple    name
    else deriveCountableComposite name
