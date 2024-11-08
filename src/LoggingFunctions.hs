{-# LANGUAGE TemplateHaskell #-}
module LoggingFunctions where

import Language.Haskell.TH
import GHC.ExecutionStack (Location(functionName))
import Data.Aeson (Value(Bool))
import Control.Monad (replicateM)

test1 :: Bool -> Bool
test1 = not

test2 :: Bool -> Int -> Bool
test2 b n = if n > 0 then b else not b

loggingName :: String -> Name
loggingName funName = mkName $ funName ++ "Logging"

makeLoggingBody :: String -> [Name] -> Q Exp
makeLoggingBody f xs = do
    let call = foldl appE (varE $ mkName f) (varE <$> xs)
    [|
        do
            let inputS = "Input: " ++ show $(tupE (varE <$> xs))
            let outputS = "Output: " ++ show $(call)
            (inputS ++ " " ++ outputS, $call)
        |]

makeLoggingDec :: String -> Q Dec
makeLoggingDec funName = do
    info <- reifyType (mkName funName)
    let (inpt, _) = splitArguments info
    let n = length inpt
    let name = loggingName funName
    xs <- replicateM n (newName "x")
    
    funD name [clause (map varP xs) (normalB $ makeLoggingBody funName xs) []]


makeLoggingUntyped :: String -> Q [Dec]
makeLoggingUntyped funName = return <$> makeLoggingDec funName

makeLoggingSig :: String -> Q Dec
makeLoggingSig funName = do
    info <- reifyType (mkName funName)
    let (inpt, outpt) = splitArguments info
    let name = loggingName funName

    let res = helpfunc (return <$> inpt) (return (AppT (AppT (TupleT 2) (ConT ''String)) outpt))
    sigD name (forallT [] (cxt []) res) 
    where 
        helpfunc xs y = foldr (\a b -> [t| $a -> $b |]) y xs

splitArguments :: Type -> ([Type], Type)
splitArguments (AppT (AppT ArrowT f_el) rest) = do
    let (input, out) = splitArguments rest
    (f_el : input, out)
splitArguments info = ([], info)

makeLogging :: String -> Q [Dec]
makeLogging funName = sequence [makeLoggingSig funName, makeLoggingDec funName]

makeListLogging :: [String] ->  Q [Dec]
makeListLogging lst = concat <$> mapM makeLogging lst