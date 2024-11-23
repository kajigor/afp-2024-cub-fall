{-# LANGUAGE TemplateHaskell #-}
module E2_LoggingExercise where

import Language.Haskell.TH
import Control.Monad (replicateM)
import Data.Bifunctor (first)
import Text.Printf (printf)

generateLoggingFunctionName :: Name -> Name
generateLoggingFunctionName originalName = mkName $ nameBase originalName ++ "Logged"

generateLoggingFunctionSig :: Name -> Q Dec
generateLoggingFunctionSig originalName = do
    t <- reifyType originalName
    sigD (generateLoggingFunctionName originalName) $ changeRetType t (\retType -> [t| (String, $retType) |])

generateLoggingFunctionBody :: Name -> [Name] -> Q Exp
generateLoggingFunctionBody original args = do
    let call = foldl appE (varE original) $ varE <$> args
    [|
        do
            let a = show $(tupE (varE <$> args))
            let res = $call
            ($(varE 'printf) "%s%s = %s\n" $(litE $ stringL $ nameBase original) a (show res), res)
        |]

generateLoggingFunctionDec :: Name -> Q Dec
generateLoggingFunctionDec origName = do
  n <- arity <$> reifyType origName
  argsNames <- replicateM n (newName "x")
  funD
    (generateLoggingFunctionName origName)
    [clause (varP <$> argsNames) (normalB $ generateLoggingFunctionBody origName argsNames) []]

generateLoggingFunctionUntyped :: Name -> Q [Dec]
generateLoggingFunctionUntyped name = return <$> generateLoggingFunctionDec name

generateLoggingFunction :: Name -> Q [Dec]
generateLoggingFunction name = sequence [generateLoggingFunctionSig name, generateLoggingFunctionDec name]

generateLoggingFunctions :: [Name] -> Q [Dec]
generateLoggingFunctions names = concat <$> mapM generateLoggingFunction names

changeRetType :: Type -> (Q Type -> Q Type) -> Q Type
changeRetType t f = case t of
    (AppT (AppT ArrowT arg) xs) -> AppT (AppT ArrowT arg) <$> changeRetType xs f
    (ForallVisT ys xs) -> ForallVisT ys <$> changeRetType xs f
    (ForallT ys ctx xs) -> ForallT ys ctx <$> changeRetType xs f
    x -> f $ return x

arity :: Type -> Int
arity t = case t of
    (AppT (AppT ArrowT arg) xs) -> 1 + arity xs
    (ForallVisT ys xs) -> arity xs
    (ForallT ys ctx xs) -> arity xs
    _ -> 0
