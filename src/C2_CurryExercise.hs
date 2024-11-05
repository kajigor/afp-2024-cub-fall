{-# LANGUAGE TemplateHaskell #-}

module C2_CurryExercise where

import Control.Monad
import Language.Haskell.TH

-- task 1: Implement generalized curry splice (inferred types)

curryName :: Int -> Name
curryName n = mkName $ "curry" ++ show n

makeCurryBody :: Name -> [Name] -> Q Exp
makeCurryBody f args = appE (varE f) (tupE (varE <$> args))

makeCurryDec :: Int -> Q Dec
makeCurryDec n = do
  f <- newName "f"
  args <- replicateM n (newName "x")
  funD (curryName n) [clause (varP f : (varP <$> args)) (normalB $ makeCurryBody f args) []]

makeCurryUntyped :: Int -> Q [Dec]
makeCurryUntyped n = return <$> makeCurryDec n

-- task 2: Add explicity signature to curry

makeCurrySig :: Int -> Q Dec
makeCurrySig n = do
  fRet <- newName "r"
  fArgs <- replicateM n (newName "a")
  let fRetType = VarT fRet
  let fArgTypes = VarT <$> fArgs
  let fArgTuple = foldl AppT (TupleT n) fArgTypes
  let fType = AppT (AppT ArrowT fArgTuple) fRetType
  let retType = foldr (AppT . AppT ArrowT) fRetType fArgTypes
  let tvs = (`PlainTV` InferredSpec) <$> (fRet : fArgs)
  let signature = ForallT tvs [] (AppT (AppT ArrowT fType) retType)
  return $ SigD (curryName n) signature

makeCurry :: Int -> Q [Dec]
makeCurry n = do
  sig <- makeCurrySig n
  dec <- makeCurryDec n
  return [sig, dec]

-- (bonus) task 3: Do the same for generalized uncurry