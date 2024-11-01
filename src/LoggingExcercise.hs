{-# LANGUAGE TemplateHaskell #-}
module LoggingExcercise where 

import Language.Haskell.TH
import Control.Monad

-- task 4: generateLoggingFunctions

generateLoggingFunctions :: [Name] -> Q [Dec]
generateLoggingFunctions names = concat <$> mapM generateLoggingFunction names

generateLoggingFunction :: Name -> Q [Dec]
generateLoggingFunction name = sequence [generateLoggingFunctionSig name, generateLoggingFunctionDec name]

generateLoggingFunctionUntyped :: Name -> Q [Dec]
generateLoggingFunctionUntyped name = return <$> generateLoggingFunctionDec name

generateLoggingFunctionDec :: Name -> Q Dec
generateLoggingFunctionDec name = do 
    VarI _ ty _ <- reify name
    (argTypes, retType) <- getArgAndResultTypes ty

    let loggedName = mkName $ "logged_" ++ nameBase name
    argNames <- replicateM (length argTypes) (newName "arg")

    let argLogging = map varE argNames
    let argsLogExp = listE argLogging

    let callFun = foldl appE (varE name) (varE <$> argNames) 

    funD loggedName [clause (map varP argNames) (normalB [|
        do 
            putStrLn $ "Input values of " ++ $(litE (stringL (nameBase name))) ++ " are: " ++ show $(argsLogExp)
            let result = $(callFun)
            putStrLn $ "Output value of " ++ $(litE (stringL (nameBase name))) ++ " is: " ++ show result 
            return result
        |]) []]

generateLoggingFunctionSig :: Name -> Q Dec
generateLoggingFunctionSig name = do 
    VarI _ ty _ <- reify name
    (argTypes, retType) <- getArgAndResultTypes ty

    let loggedName = mkName $ "logged_" ++ nameBase name
    let fOut = func (return <$> argTypes) [t| IO $(return retType) |]

    sigD loggedName 
        (
            forallT [] 
                (cxt []) 
            [t| $fOut |]
        )
    where
        func xs y = foldr (\a b -> [t| $a -> $b |]) y xs
    
getArgAndResultTypes :: Type -> Q ([Type], Type)
getArgAndResultTypes (AppT (AppT ArrowT arg) rest) = do 
    (args, result) <- getArgAndResultTypes rest
    return (arg : args, result)
getArgAndResultTypes resultType = return ([], resultType)

