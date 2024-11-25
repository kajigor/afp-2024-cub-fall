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

test3 :: Show a => a -> [a] 
test3 = replicate 42 

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
    let (_, _, inpt, _) = splitArguments info
    let n = length inpt
    let name = loggingName funName
    xs <- replicateM n (newName "x")
    
    funD name [clause (map varP xs) (normalB $ makeLoggingBody funName xs) []]


makeLoggingUntyped :: String -> Q [Dec]
makeLoggingUntyped funName = return <$> makeLoggingDec funName

makeLoggingSig :: String -> Q Dec
makeLoggingSig funName = do
    info <- reifyType (mkName funName)
    let (cur_cxt, forallVars, inpt, outpt) = splitArguments info
    let name = loggingName funName

    let res = helpfunc (return <$> inpt) (return (AppT (AppT (TupleT 2) (ConT ''String)) outpt))
    sigD name (forallT forallVars (return cur_cxt) res) 
    where 
        helpfunc xs y = foldr (\a b -> [t| $a -> $b |]) y xs

-- need to return all elements, which is 
-- return forall, input, output
splitArguments :: Type -> (Cxt, [TyVarBndr Specificity], [Type], Type)
splitArguments (ForallVisT elst tp) = let (cxts, forallVars, inpt, outp) = splitArguments tp
    in (cxts, map toSpecificity elst ++ forallVars, inpt, outp)
splitArguments (ForallT elst eq tp) = let (cxts, forallVars, inpt, outp) = splitArguments tp
    in (eq ++ cxts, elst ++ forallVars, inpt, outp)
splitArguments (AppT (AppT ArrowT f_el) rest) = do
    let (cxts, forallVars, input, out) = splitArguments rest
    (cxts, forallVars, f_el : input, out)
splitArguments info = ([], [], [], info)

toSpecificity :: TyVarBndr () -> TyVarBndr Specificity
toSpecificity (PlainTV name ()) = PlainTV name SpecifiedSpec
toSpecificity (KindedTV name () knd) = KindedTV name SpecifiedSpec knd

makeLogging :: String -> Q [Dec]
makeLogging funName = sequence [makeLoggingSig funName, makeLoggingDec funName]

makeListLogging :: [String] ->  Q [Dec]
makeListLogging lst = concat <$> mapM makeLogging lst