{-# LANGUAGE TemplateHaskell #-}

module C2_CurryExercise where

import Control.Monad (guard)
import GHC.ExecutionStack (Location (functionName))
import Language.Haskell.TH

-- task 1: Implement generalized curry splice (inferred types)

curryName :: Int -> Name
curryName n = mkName $ "curry" ++ show n

makeCurryBody :: Int -> Q Exp
makeCurryBody n = undefined

makeCurryDec :: Int -> Q Dec
makeCurryDec n = undefined

makeCurryUntyped :: Int -> Q [Dec]
makeCurryUntyped n = undefined

-- task 2: Add explicity signature to curry

makeCurrySig :: Int -> Q Dec
makeCurrySig = undefined

makeCurry :: Int -> Q [Dec]
makeCurry n = undefined

-- (bonus) task 3: Do the same for generalized uncurry

uncurryName :: Int -> Name
uncurryName n = mkName $ "uncurry" ++ show n

makeUncurryDec :: Int -> Q Dec
makeUncurryDec n = do
  let argNames = [mkName $ "x" ++ show i | i <- [1 .. n]]
  let funName = mkName "f"

  let tupleArg = TupP $ map VarP argNames
  let funArg = VarP $ mkName "f"

  let expr = foldl AppE (VarE funName) (map VarE argNames)

  return $ FunD (uncurryName n) [Clause [funArg, tupleArg] (NormalB expr) []]

makeUncurrySig :: Int -> Q Dec
makeUncurrySig n = do
  let argNames = [mkName $ "x" ++ show i | i <- [1 .. n]]
  let retName = mkName "r"

  let tupleTy = foldl AppT (TupleT n) (map VarT argNames)
  let funTy = foldr ((AppT . AppT ArrowT) . VarT) (VarT retName) argNames
  let retTy = VarT retName

  SigD (uncurryName n) . ForallT (map (`PlainTV` SpecifiedSpec) (retName : argNames)) [] <$> [t|$(return funTy) -> $(return tupleTy) -> $(return retTy)|]

makeUncurry :: Int -> Q [Dec]
makeUncurry n = do
  sig <- makeUncurrySig n
  dec <- makeUncurryDec n
  return [sig, dec]

-- Create a Template Haskell function `generateLoggingFunctions` that takes a list of function names and generates new functions which log their inputs and outputs.

-- test functions:
testF0 :: Int
testF0 = 42

testF1 :: Int -> Int
testF1 x = x + 1

testF2 :: String -> Float -> Int
testF2 _ _ = 42

testFIO :: Int -> Int -> Int -> IO Int
testFIO x y z = return $ x + y + z

generateWrapLogNName :: Int -> Name
generateWrapLogNName n = mkName $ "wrapLog" ++ show n

generateWrapLogNBody :: String -> Int -> Q Dec
generateWrapLogNBody outerFunName n = do
  let funName = mkName "f"
  let argNames = [mkName $ "a" ++ show i | i <- [1 .. n]]
  let resultName = mkName "result"

  let funPat = VarP funName
  let funExpr = VarE funName
  let argPats = map VarP argNames
  let argExprs = map VarE argNames

  let logFunStr = LitE $ StringL $ "Calling " ++ outerFunName ++ " with arguments:"
  let logArgsStrs = map (AppE (VarE 'show)) argExprs
  let argsLogStmts = map (AppE (VarE 'putStrLn)) (logFunStr : logArgsStrs)

  let funCallExpr = foldl AppE funExpr argExprs
  let funCallStmt = LetS [ValD (VarP resultName) (NormalB funCallExpr) []]

  logResultStr <- [|putStrLn ("Result: " ++ show $(return $ VarE resultName))|]

  let returnStmt = AppE (VarE 'return) (VarE resultName)

  let body = DoE Nothing $ map NoBindS argsLogStmts ++ [funCallStmt, NoBindS logResultStr, NoBindS returnStmt]

  return $ FunD (generateWrapLogNName n) [Clause (funPat : argPats) (NormalB body) []]

generateLoggedFunctionBody :: String -> Q Dec
generateLoggedFunctionBody name = do
  let funName = mkName name
  info <- reify funName

  funTy <- case info of
    VarI _ ty _ -> return ty
    _ -> error "Invalid function name"

  arity <- getArity funTy

  wrapLogNDecl <- generateWrapLogNBody name arity
  let wrapLogNName = generateWrapLogNName arity

  let loggedFunName = mkName $ name ++ "Logged"

  return $ FunD loggedFunName [Clause [] (NormalB $ AppE (VarE wrapLogNName) (VarE funName)) [wrapLogNDecl]]
  where
    getArity :: (MonadFail m) => Type -> m Int
    getArity (AppT (AppT ArrowT _) b) = do
      n <- getArity b
      return $ n + 1
    getArity (ForallT _ _ ty) = getArity ty
    getArity _ = return 0

generateLoggedFunctionSig :: String -> Q Dec
generateLoggedFunctionSig name = do
  let funName = mkName name
  info <- reify funName

  funTy <- case info of
    VarI _ ty _ -> return ty
    _ -> error "Invalid function name"

  let loggedFunName = mkName $ name ++ "Logged"

  return $ SigD loggedFunName $ insertIO funTy
  where
    insertIO :: Type -> Type
    insertIO (AppT (AppT ArrowT a) b) = AppT (AppT ArrowT a) (insertIO b)
    insertIO (ForallT tvs ctx ty) = ForallT tvs ctx (insertIO ty)
    insertIO a = AppT (ConT ''IO) a

generateLoggedFunction :: String -> Q [Dec]
generateLoggedFunction name = do
  body <- generateLoggedFunctionBody name
  sig <- generateLoggedFunctionSig name
  return [sig, body]
