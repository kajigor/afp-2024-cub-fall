{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module GenLog where

import Control.Monad
import Data.List (nub, unfoldr)
import Language.Haskell.TH

genLogFuncBody :: Name -> [Name] -> Type -> Q Exp
genLogFuncBody nm argsN retT =
  let paramsQ = listE (stringE (nameBase nm) : (appE (varE 'show) . varE <$> argsN))
      callQ = foldl appE (varE nm) (varE <$> argsN)
   in [|
        do
          let call = unwords $(paramsQ)
          putStrLn $ "Calling (" ++ call ++ ")"
          res <- $(if returnsIO retT then callQ else [|pure $(callQ)|])
          putStrLn $ "Returning (" ++ call ++ ") -> " ++ show res
          return res
        |]

genLogFuncDec :: Name -> Name -> Type -> Q Dec
genLogFuncDec logNm nm fT = do
  fArity <- arity fT
  argsN <- replicateM fArity (newName "arg")
  funD logNm [clause (map varP argsN) (normalB $ genLogFuncBody nm argsN fT) []]

genLogFuncSign :: Name -> Type -> Dec
genLogFuncSign nm fT =
  let preds = showPred <$> collectTVNames fT
      ftIO = makeReturnIO fT
   in SigD nm $ applyPreds ftIO preds

genLogFunc :: Name -> Q [Dec]
genLogFunc f = do
  fT <- genValueType f
  let name = mkName $ nameBase f ++ "Log"
  let sign = genLogFuncSign name fT
  dec <- genLogFuncDec name f fT
  return [sign, dec]

{-
Note: Handles functions returning IO
TODO: Support for functions with type variables
-}
genLogFuncs :: [Name] -> Q [Dec]
genLogFuncs = fmap concat . mapM genLogFunc

genValueType :: Name -> Q Type
genValueType nm = do
  info <- reify nm
  case info of
    VarI _ t _ -> return t
    _ -> fail $ "Not a value: " ++ show nm

-- Utility functions

applyPreds :: Type -> [Pred] -> Type
applyPreds (ForallT tvs ctx t) preds = ForallT tvs (nub (ctx ++ preds)) t
applyPreds t preds = ForallT [] (nub preds) t

showPred :: Name -> Pred
showPred n = AppT (ConT ''Show) (VarT n)

collectTVNames :: Type -> [Name]
collectTVNames (ForallT tvs _ t) = tvName <$> tvs
  where
    tvName (PlainTV n _) = n
    tvName (KindedTV n _ _) = n
collectTVNames _ = []

returnsIO :: Type -> Bool
returnsIO (ForallT _ _ t) = returnsIO t
returnsIO (AppT (AppT ArrowT _) t) = returnsIO t
returnsIO (AppT (ConT n) _) = n == ''IO
returnsIO _ = False

makeReturnIO :: Type -> Type
makeReturnIO (ForallT tvs ctx t) = ForallT tvs ctx $ makeReturnIO t
makeReturnIO (AppT (AppT ArrowT a) r) = AppT (AppT ArrowT a) $ makeReturnIO r
makeReturnIO (AppT (ConT n) t) =
  if n == ''IO
    then AppT (ConT n) t
    else AppT (ConT ''IO) (AppT (ConT n) t)
makeReturnIO t = AppT (ConT ''IO) t

arity :: Type -> Q Int
arity t =
  let result = count t
   in if result == 0
        then fail $ "Not a function type: " ++ show t
        else return result
  where
    count :: Type -> Int
    count (ForallT _ _ typ) = count typ
    count (AppT (AppT ArrowT _) typ) = 1 + count typ
    count _ = 0
