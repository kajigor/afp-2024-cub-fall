{-# LANGUAGE TemplateHaskell #-}
module Logging where

import Language.Haskell.TH
import Control.Monad

generateLoggingFunctions :: [Name] -> Q [Dec]
generateLoggingFunctions funcNames = concat <$> mapM generateSingleLoggingFunction funcNames

generateSingleLoggingFunction :: Name -> Q [Dec]
generateSingleLoggingFunction funcName = do
    funcInfo <- reify funcName
    case funcInfo of
        VarI _ funcType _ -> do
            (funcArgs, returnType) <- getFuncArgs funcType
            argNames <- replicateM (length funcArgs) (newName "arg")
            let logFuncName = mkName $ "log_" ++ nameBase funcName
            logBody <- normalB [|
                do
                    putStrLn $ "Input values of " ++ $(litE (stringL (nameBase funcName))) ++ ": " ++ show $(listE $ map varE argNames)
                    let result = $(foldl appE (varE funcName) (varE <$> argNames))
                    putStrLn $ "Output value of " ++ $(litE (stringL (nameBase funcName))) ++ ": " ++ show result
                    return result
                |]
            return [FunD logFuncName [Clause (map VarP argNames) logBody []]]
        _ -> fail $ "Expected function name, but got: " ++ show funcName

getFuncArgs :: Type -> Q ([Type], Type)
getFuncArgs (AppT (AppT ArrowT arg) rest) = do
    (args, result) <- getFuncArgs rest
    return (arg : args, result)
getFuncArgs resultType = return ([], resultType)
