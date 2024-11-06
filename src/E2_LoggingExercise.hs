{-# LANGUAGE TemplateHaskell #-}
module E2_LoggingExercise where

import Language.Haskell.TH
import Control.Monad (replicateM)
import Data.Bifunctor (first)

generateLoggingFunctionName :: Name -> Name
generateLoggingFunctionName originalName = mkName $ nameBase originalName ++ "Logged"

generateLoggingFunctionSig :: Name -> Q Dec
generateLoggingFunctionSig originalName = do
    (argsTypes, retType) <- split <$> reifyType originalName
    sigD (generateLoggingFunctionName originalName) $ func (return <$> argsTypes) [t| ($(return retType), String) |]
    where
        func xs y = foldr (\a b -> [t| $a -> $b |]) y xs

generateLoggingFunctionBody :: Name -> [Name] -> Q Exp
generateLoggingFunctionBody original args = do
    let call = foldl appE (varE original) $ varE <$> args
    [|
        do
            let a = show $(listE (varE <$> args))
            let res = $call
            (res, $(litE $ stringL $ nameBase original) <> " " <> a <> " " <> show res)
        |]

generateLoggingFunctionDec :: Name -> Q Dec
generateLoggingFunctionDec origName = do
  (origArgsNames, _) <- split <$> reifyType origName
  let n = length origArgsNames
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

split :: Type -> ([Type], Type)
split (AppT (AppT ArrowT arg) xs) = first (arg :) $ split xs
split x = ([], x)