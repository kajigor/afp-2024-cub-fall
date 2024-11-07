{-# LANGUAGE TemplateHaskell #-}

module E2_LoggingExercise where

import Language.Haskell.TH
import Control.Monad

generateLoggingFunctionName :: Name -> Name
generateLoggingFunctionName name = mkName $ nameBase name ++ "Logged"

generateLoggingFunctionBody :: Name -> [Name] -> Q Exp
generateLoggingFunctionBody name args =
    [|
            let result = $(foldl appE (varE name) $ varE <$> args)
                pName = $(litE (stringL (nameBase name)))
                pCallArgs = show $(listE $ appE (varE 'show) .varE <$> args)
            in
            (
                result, 
                "Called function `" <> pName <> "` with args `" <> pCallArgs <> "'. " <>
                 "Function output: `" <> show result <> "`"
            )
    |]

generateLoggingFunctionDec :: Name -> Q Dec
generateLoggingFunctionDec fnName = do
    tp <- reifyType fnName
    (args, _) <- splitFunc tp
    argsNames <- replicateM (length args) (newName "arg")
    funD (generateLoggingFunctionName fnName) [clause (map varP argsNames) (normalB $ generateLoggingFunctionBody fnName argsNames) []]

generateLoggingFunctionUntyped :: Name -> Q [Dec]
generateLoggingFunctionUntyped name = return <$> generateLoggingFunctionDec name

generateLoggingFunctionSig :: Name -> Q Dec
generateLoggingFunctionSig fnName = do
    tp <- reifyType fnName
    (args, fn) <- splitFunc tp
    sigD (generateLoggingFunctionName fnName) $ foldr ((\a b -> [t| $a -> $b |]) . return) [t| ($(return fn), String) |] args


generateLoggingFunction :: Name -> Q [Dec]
generateLoggingFunction name = sequence [generateLoggingFunctionSig name, generateLoggingFunctionDec name]

generateLoggingFunctions :: [Name] -> DecsQ
generateLoggingFunctions names = concat <$> mapM generateLoggingFunction names

splitFunc :: Type -> Q ([Type], Type)
splitFunc t = return $ go t []
  where
    go (AppT (AppT ArrowT arg) rest) args = go rest (args ++ [arg])
    go resultType args = (args, resultType)