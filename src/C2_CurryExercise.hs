{-# LANGUAGE TemplateHaskell #-}
module C2_CurryExercise where

import Language.Haskell.TH
import Control.Monad

-- task 1: Implement generalized curry splice (inferred types)

curryName :: Int -> Name
curryName n = mkName $ "curry" ++ show n

makeCurryBody :: Name -> [Name] -> Int -> Q Exp
makeCurryBody f xs n = appE (varE f) (tupE $ varE <$> xs)

makeCurryDec :: Int -> Q Dec
makeCurryDec n = do
    f  <- newName "f"
    xs <- replicateM n (newName "x")
    funD (curryName n) [clause (varP f : (varP <$> xs)) (normalB $ makeCurryBody f xs n) []]

makeCurryUntyped :: Int -> Q [Dec]
makeCurryUntyped n = return <$> makeCurryDec n

-- task 2: Add explicity signature to curry

makeCurrySig :: Int -> Q Dec
makeCurrySig n = do
  xs <- replicateM n (newName "x")
  let rType = VarT $ mkName "y"
  let aTypes = map VarT xs 

  let tupleType = foldl AppT (TupleT n) aTypes
  let funcType = AppT (AppT ArrowT tupleType) rType
  let curriedType = foldr (AppT . AppT ArrowT) rType aTypes
  let fullType = AppT (AppT ArrowT funcType) curriedType

  return $ SigD (curryName n) fullType

makeCurry :: Int -> Q [Dec]
makeCurry n = sequence [makeCurrySig n, makeCurryDec n]

-- (bonus) task 3: Do the same for generalized uncurry

uncurryName :: Int -> Name
uncurryName n = mkName $ "uncurry" ++ show n

makeUncurryBody :: Int -> Name -> [Name] -> Q Exp
makeUncurryBody n f xs = foldl appE (varE f) (varE <$> xs) 

makeUncurryDec :: Int -> Q Dec
makeUncurryDec n = do
    f <- newName "f"
    xs <- replicateM n (newName "x")
    funD (uncurryName n) [clause [varP f, tupP (varP <$> xs)] (normalB $ makeUncurryBody n f xs) []]

makeUncurryUntyped :: Int -> Q [Dec]
makeUncurryUntyped n = return <$> makeUncurryDec n 

makeUncurrySig :: Int -> Q Dec
makeUncurrySig n = do 
    xs <- replicateM n (newName "x")
    let rType = VarT $ mkName "y"
    let aTypes = map VarT xs 

    let tupleType = foldl AppT (TupleT n) aTypes
    let curriedType = foldr (AppT . AppT ArrowT) rType aTypes
    let fullType = AppT (AppT ArrowT curriedType) (AppT (AppT ArrowT tupleType) rType)

    return $ SigD (uncurryName n) fullType

makeUncurry :: Int -> Q [Dec]
makeUncurry n = sequence [makeUncurrySig n, makeUncurryDec n]