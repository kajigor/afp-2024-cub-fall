{-# LANGUAGE TemplateHaskell #-}
module E2_LoggedExercise where 

import Language.Haskell.TH
import Control.Monad

generateLoggingFunctions :: [Name] -> Q [Dec]
generateLoggingFunctions = mapM generateLoggingFunction

generateLoggingFunction :: Name -> Q Dec
generateLoggingFunction name = do
    type' <- reifyType name
    args <- replicateM (argsCount type') $ newName "x"
    body <- normalB [| do
        putStrLn $ "Input: " ++ show $(listE $ varE <$> args)
        let res = $(foldl appE (varE name) (varE <$> args))
        putStrLn $ "Output: " ++ show res
        return res
        |]
    let loggedName = mkName $ nameBase name ++ "Logged"
    return $ FunD loggedName [Clause (VarP <$> args) body []]

argsCount :: Type -> Int
argsCount (AppT (AppT ArrowT _) rest) = 1 + argsCount rest
argsCount _                           = 0
