{-# LANGUAGE TemplateHaskell #-}
module C2_CurryExercise where

import Language.Haskell.TH
import Control.Monad (replicateM)

-- task 1: Implement generalized curry splice (inferred types)

curryName :: Int -> Name
curryName n = mkName $ "curry" ++ show n

makeCurryBody :: Name -> [Name] -> Int -> Q Exp
makeCurryBody f xs n = appE (varE f) (tupE $ varE <$> xs)

makeCurryDec :: Int -> Q Dec
makeCurryDec n = do
    f  <- newName "f"
    xs <- replicateM n (newName "x")
    funD (curryName n) [clause (varP f:(varP <$> xs)) (normalB $ makeCurryBody f xs n) []]

makeCurryUntyped :: Int -> Q [Dec]
makeCurryUntyped n = return <$> makeCurryDec n


-- task 2: Add explicity signature to curry

makeCurrySig :: Int -> Q Dec
makeCurrySig n = do
    let y = mkName "y"
    xs <- replicateM n (newName "x")
    let fIn = [t| $(foldl appT (tupleT n) (varT <$> xs)) -> $(varT y) |]
    let fOut = func (varT <$> xs) (varT y)
    sigD (curryName n)
        (
            forallT (tv y : (tv <$> xs))
            (cxt [] )
            [t| $fIn -> $fOut |]
        )
    where
        tv x = PlainTV x specifiedSpec
        func xs y = foldr (\a b -> [t| $a -> $b |]) y xs


makeCurry :: Int -> Q [Dec]
makeCurry n = sequence [makeCurrySig n, makeCurryDec n]



-- (bonus) task 3: Do the same for generalized uncurry

uncurryName :: Int -> Name
uncurryName n = mkName $ "uncurry" ++ show n

makeUncurryBody :: Name -> [Name] -> Int -> Q Exp
makeUncurryBody f xs n = foldl appE (varE f) (varE <$> xs)

makeUncurryDec :: Int -> Q Dec
makeUncurryDec n = do
    f  <- newName "f"
    xs <- replicateM n (newName "x")
    funD (uncurryName n) [clause [varP f, tupP (varP <$> xs)] (normalB $ makeUncurryBody f xs n) []]

makeUncurryUntyped :: Int -> Q [Dec]
makeUncurryUntyped n = return <$> makeUncurryDec n

makeUncurrySig :: Int -> Q Dec
makeUncurrySig n = do
    let y = mkName "y"
    xs <- replicateM n (newName "x")
    let fIn = func (varT <$> xs) (varT y)
    let fOut = [t| $(foldl appT (tupleT n) (varT <$> xs)) -> $(varT y) |]
    sigD (uncurryName n)
        (
            forallT (tv y : (tv <$> xs))
            (cxt [] )
            [t| $fIn -> $fOut |]
        )
    where
        tv x = PlainTV x specifiedSpec
        func xs y = foldr (\a b -> [t| $a -> $b |]) y xs

makeUncurry :: Int -> Q [Dec]
makeUncurry n = sequence [makeUncurrySig n, makeUncurryDec n]
