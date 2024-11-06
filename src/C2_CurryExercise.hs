{-# LANGUAGE TemplateHaskell #-}
module C2_CurryExercise where 

import Language.Haskell.TH
import Control.Monad (replicateM)

-- task 1: Implement generalized curry splice (inferred types)

curryName :: Int -> Name
curryName n = mkName $ "curry" ++ show n

makeCurryBody :: Name -> [Name] -> Q Exp
makeCurryBody f xs = appE (varE f) (tupE $ varE <$> xs)

makeCurryDec :: Int -> Q Dec
makeCurryDec n = undefined

makeCurryUntyped :: Int -> Q [Dec]
makeCurryUntyped n = undefined


-- task 2: Add explicity signature to curry

makeCurrySig :: Int -> Q Dec
makeCurrySig = undefined

makeCurry :: Int -> Q [Dec]
makeCurry n = undefined

-- task 3

uncurryName :: Int -> Name
uncurryName n = mkName $ "uncurry" ++ show n

makeUncurryBody :: Name -> [Name] -> Q Exp
makeUncurryBody f el = foldl appE (varE f) (varE <$> el)

makeUncurryDec :: Int -> Q Dec
makeUncurryDec n = do
    f <- newName "f"
    xs <- replicateM n (newName "x")
    funD (uncurryName n) [clause [varP f , tupP (varP <$> xs)] (normalB $ makeUncurryBody f xs) []]

makeUncurryUntyped :: Int -> Q [Dec]
makeUncurryUntyped n = return <$> makeUncurryDec n

-- uncurry3 :: (a -> b -> c -> y) -> ((a, b, c) -> y)
makeUncurrySig :: Int -> Q Dec
makeUncurrySig n = do
    let name = uncurryName n
    y <- newName "y"
    xs <- replicateM n (newName "x")
    let fIn = helpfunc (varT <$> xs) (varT y)
    let fOut = [t| $(foldl appT (tupleT n) (varT <$> xs)) -> $(varT y) |]
    let res = [t| $fIn -> $fOut |]
    sigD name (forallT (tv y : (tv <$> xs)) (cxt []) res) 
    where 
        tv x = PlainTV x specifiedSpec
        helpfunc xs y = foldr (\a b -> [t| $a -> $b |]) y xs

makeUncurry :: Int -> Q [Dec]
makeUncurry n = sequence [makeUncurrySig n, makeUncurryDec n]