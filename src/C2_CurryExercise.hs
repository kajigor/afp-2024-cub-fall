{-# LANGUAGE TemplateHaskell #-}
module C2_CurryExercise where 

import Language.Haskell.TH
import Control.Monad

-- task 1: Implement generalized curry splice (inferred types)

curryName :: Int -> Name
curryName n = mkName $ "curry" ++ show n

makeCurryBody :: Int -> Name -> [Name] -> Q Exp
-- ((x0, x1, x2, x3, ...) -> y) -> x0 -> .. -> xn -> y
makeCurryBody n f xs = do 
    return $ AppE (VarE f) (TupE (Just . VarE <$> xs))

makeCurryDec :: Int -> Q Dec
makeCurryDec n = do
    f <- newName "f" -- f
    xs <- replicateM n (newName "x") -- x0, x1, ..., xn, 
    funD (curryName n) [clause (varP f:(varP <$> xs)) (normalB $ makeCurryBody n f xs) []]

makeCurryUntyped :: Int -> Q [Dec]
makeCurryUntyped n = return <$> makeCurryDec n


-- task 2: Add explicity signature to curry

makeCurrySig :: Int -> Q Dec
makeCurrySig n = do 
    xs <- replicateM n (newName "x")
    let y = mkName "y"
    let tuple = foldl (\a b -> appT a (varT b)) (tupleT n) xs 
    
    let fIn = func [tuple] (varT y)

    let fOut = func (varT <$> xs) (varT y) 

    sigD (curryName n) -- curryNamen :: 
        (
            forallT (tv y : (tv <$> xs)) -- forall y x0 x1 ... xn. 
                (cxt [])
            [t| $fIn -> $fOut |] -- (x0 -> x1 -> ... -> xn -> y) -> (x0 -> x1 -> ... -> xn -> y)
        )
    where
        tv x = PlainTV x specifiedSpec
        -- (x0 -> (x1 -> ()... -> (xn -> y))))
        func xs y = foldr (\a b -> [t| $a -> $b |]) y xs

makeCurry :: Int -> Q [Dec]
makeCurry n = sequence [makeCurrySig n, makeCurryDec n]


-- (bonus) task 3: Do the same for generalized uncurry


uncurryName :: Int -> Name
uncurryName n = mkName $ "uncurry" ++ show n

makeUncurryBody :: Int -> Name -> [Name] -> Q Exp
makeUncurryBody n f xs = foldl appE (varE f) (varE <$> xs) 

makeUncurryDec :: Int -> Q Dec
-- (x0 -> .. -> xn -> y) -> (x0, ..., xn) -> y
makeUncurryDec n = do
    f <- newName "f" -- f
    xs <- replicateM n (newName "x") 
    let tuplePat = TupP (VarP <$> xs) 

    funD (uncurryName n) [clause [varP f, tupP (varP <$> xs)] (normalB $ makeUncurryBody n f xs) []]

makeUncurryUntyped :: Int -> Q [Dec]
makeUncurryUntyped n = return <$> makeUncurryDec n

makeUncurrySig :: Int -> Q Dec
makeUncurrySig n = do 
    xs <- replicateM n (newName "x")
    let y = mkName "y"
    let tuple = foldl (\a b -> appT a (varT b)) (tupleT n) xs 

    let fIn = func (varT <$> xs) (varT y) 
    
    let fOut = func [tuple] (varT y)

    sigD (uncurryName n) -- curryNamen :: 
        (
            forallT (tv y : (tv <$> xs)) -- forall y x0 x1 ... xn. 
                (cxt [])
            [t| $fIn -> $fOut |] -- (x0 -> x1 -> ... -> xn -> y) -> ((x0, ..., xn) -> y)
        )
    where
        tv x = PlainTV x specifiedSpec
        -- (x0 -> (x1 -> ()... -> (xn -> y))))
        func xs y = foldr (\a b -> [t| $a -> $b |]) y xs


makeUncurry :: Int -> Q [Dec]
makeUncurry n = sequence [makeUncurrySig n, makeUncurryDec n]


