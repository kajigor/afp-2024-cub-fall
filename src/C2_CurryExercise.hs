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
  let fRetType = varT fRet
  let fArgTypes = varT <$> fArgs
  let fArgTuple = foldl appT (tupleT n) fArgTypes
  let fType = appT (appT arrowT fArgTuple) fRetType
  let retType = foldr (appT . appT arrowT) fRetType fArgTypes
  tvs <- traverse (`plainInvisTV` InferredSpec) (fRet : fArgs)
  let signature = forallT tvs (cxt []) (appT (appT arrowT fType) retType)
  sigD (curryName n) signature

makeCurry :: Int -> Q [Dec]
makeCurry n = do
  sig <- makeCurrySig n
  dec <- makeCurryDec n
  return [sig, dec]

-- (bonus) task 3: Do the same for generalized uncurry

uncurryName :: Int -> Name
uncurryName n = mkName $ "uncurry" ++ show n

makeUncurryBody :: Name -> [Name] -> Q Exp
makeUncurryBody f args = foldl appE (varE f) (varE <$> args)

makeUncurryDec :: Int -> Q Dec
makeUncurryDec n = do
  f <- newName "f"
  args <- replicateM n (newName "x")
  funD (uncurryName n) [clause [varP f, tupP (varP <$> args)] (normalB $ makeUncurryBody f args) []]

makeUncurryUntyped :: Int -> Q [Dec]
makeUncurryUntyped n = return <$> makeUncurryDec n

makeUncurrySig :: Int -> Q Dec
makeUncurrySig n = do
  fRet <- newName "r"
  fArgs <- replicateM n (newName "a")
  let fRetType = varT fRet
  let fArgTypes = varT <$> fArgs
  let argTuple = foldl appT (tupleT n) fArgTypes
  let fType = foldr (appT . appT arrowT) fRetType fArgTypes
  let retType = appT (appT arrowT argTuple) fRetType
  tvs <- traverse (`plainInvisTV` InferredSpec) (fRet : fArgs)
  let signature = forallT tvs (cxt []) (appT (appT arrowT fType) retType)
  sigD (uncurryName n) signature

makeUncurry :: Int -> Q [Dec]
makeUncurry n = do
  sig <- makeUncurrySig n
  dec <- makeUncurryDec n
  return [sig, dec]