{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Imp.SLang where

import Control.Monad.State (StateT (runStateT), get, put)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Class (lift)
import qualified Imp.Lang as L
import qualified Imp.TLang as TL
import Text.Read (readMaybe)
import Imp.Lang (Op)

data Z = Z

data S a = S a

data GEQ a b where
  GEQZ :: GEQ a Z -- базовое сравнение: любое a >= Z
  GEQS :: GEQ a b -> GEQ (S a) (S b) -- накрутка по S: любое S(a) больше S(b)

geqTrans :: GEQ a b -> GEQ b c -> GEQ a c
geqTrans GEQZ GEQZ = GEQZ
geqTrans (GEQS ab) GEQZ = GEQZ
geqTrans (GEQS ab) (GEQS bc) = GEQS (geqTrans ab bc)

class Numm a where
  geqRefl :: GEQ a a

instance Numm Z where
  -- geqRefl :: GEQ Z Z
  geqRefl = GEQZ

instance (Numm a) => Numm (S a) where
  -- geqRefl :: Numm a => GEQ (S a) (S a)

  -- geqRefl = GEQS (geqRefl :: GEQ a b)
  geqRefl = GEQS geqRefl

data SafeExpr t i where
  Var ::
    GEQ i (S iv) ->
    iv ->
    SafeExpr Int i
  Const ::
    Int -> SafeExpr Int i
  BinOp ::
    Op -> SafeExpr Int i -> SafeExpr Int i -> SafeExpr Int i
  Read ::
    SafeExpr Int i
  Write ::
    Show t => SafeExpr t i -> SafeExpr () i
  Seq :: -- Why () ...?
    SafeExpr () i -> SafeExpr t i -> SafeExpr t i
  If ::
    SafeExpr Int i -> SafeExpr t i -> SafeExpr t i -> SafeExpr t i
  LetIn ::
    String -> SafeExpr Int i -> SafeExpr t (S i) -> SafeExpr t i
  Skip ::
    SafeExpr () i

data Error = ParsingErr String | UndefinedVariable String
  deriving (Eq, Show)

data SizedList v s where
  Nil :: SizedList v Z
  Cons :: v -> SizedList v s -> SizedList v (S s)

type EvalM i = StateT (SizedList Int i) (ExceptT Error IO)

defineAction :: Num t => Op -> (t -> t -> t)
defineAction op = case op of
  L.Plus -> (+)
  L.Minus -> (-)


getAtInd :: GEQ l (S ind) -> SizedList Int l -> Int
getAtInd (GEQS GEQZ) (Cons h tail) = h
getAtInd (GEQS (GEQS proof)) (Cons h tail) = getAtInd (GEQS proof) tail


evalExpr :: Show t => GEQ i ie -> SafeExpr t ie -> EvalM i t
evalExpr geq (Const x) = lift $ lift $ return x

evalExpr geqE (Var geqI iv) = do
  -- В данном случае geqI -- внутреннее доказательство того, что переменная
  -- с именем iv ссылается на РЕАЛЬНОЕ значение в стеке из значений
  -- А geqE задает НОВЫЙ размер стека

  -- Проверяем, что мы не уменьшаем размер стека переменных
  let proof = geqTrans geqE geqI

  value <- Control.Monad.State.get

  let extractedValue = getAtInd proof value

  return extractedValue


evalExpr geq (BinOp op expr1 expr2) = do
  -- Все валидно, тупо вычисляем
  left <- evalExpr geq expr1
  right <- evalExpr geq expr2

  lift $ lift $ return (defineAction op left right)


evalExpr geq (Seq expr1 expr2) = do
  -- Все валидно, тупо вычисляем
  -- Возвращаем последний стейтмент
  left <- evalExpr geq expr1
  right <- evalExpr geq expr2

  lift $ lift $ return right


evalExpr geq (If cond thn els) = do
  -- Все валидно, тупо вычисляем
  condValue <- evalExpr geq cond

  branchValue <- if condValue > 0
    then evalExpr geq thn
    else evalExpr geq els

  lift $ lift $ return branchValue


evalExpr geqE (LetIn name exprValue exprIn) = do
  -- Подсчитали значение выражения name в обычном контексте
  value <- evalExpr geqE exprValue

  -- Расширяем контекст
  -- 1. Добавляем значение
  vars <- Control.Monad.State.get

  -- 2. Увеличиваем на 1 количество перменных в контексте (сравнение со вторым элементом нам не нужно)
  -- Внутри (Cons value vars)
  -- Главное, чтобы все теперь выполнялось в новом контексте
  let geqNew = GEQS geqE

  lift $ fst <$> runStateT (evalExpr geqNew exprIn) (Cons value vars)

evalExpr geq Read = do
  line <- lift $ lift getLine

  case readMaybe line of
    Just v -> lift $ lift $ return v
    Nothing -> lift $ throwE $ ParsingErr "Program can not read value"


evalExpr geq (Write expr) = do
  result <- evalExpr geq expr
  lift $ lift $ print result


evalExpr geq Skip = do
  return ()


data SafeCompileError = UndefinedVar String
  deriving (Eq, Show)

type CompileM i m = StateT (SizedList String i) (ExceptT SafeCompileError m)

-- Переменная -> Список из переменных -> Выражение в индексах
-- Идея реализации этого метода позаимстована у Mikhail Rodionychev
findByValue :: String -> SizedList String l -> Maybe (SafeExpr Int l)
findByValue value Nil = Nothing
findByValue value (Cons h tail) =
  if value == h
    then Just $ Var (GEQS GEQZ) Z
    else case findByValue h tail of
      Just (Var geq l) -> Just $ Var (GEQS geq) (S l)
      _ -> Nothing


compile :: (Monad m) => (Numm i) => TL.TExpr t -> i -> CompileM i m (SafeExpr t i)
compile (TL.Var name) len = do
  context <- get

  case findByValue name context of
    Just expr -> lift $ lift $ return expr
    Nothing -> lift $ throwE $ UndefinedVar $ name ++ " variable does not exist"

compile (TL.Const v) _ = lift $ lift $ return $ Const v

compile (TL.BinOp op expr1 expr2) len = do
  compiledExpr1 <- compile expr1 len
  compiledExpr2 <- compile expr2 len

  lift $ lift $ return $ BinOp op compiledExpr1 compiledExpr2

compile TL.Read _ = lift $ lift $ return Read

compile (TL.Write expr) len = do
  compiledExpr <- compile expr len

  lift $ lift $ return $ Write compiledExpr

compile (TL.Seq expr1 expr2) len = do
  compiledExpr1 <- compile expr1 len
  compiledExpr2 <- compile expr2 len

  lift $ lift $ return $ Seq compiledExpr1 compiledExpr2

compile (TL.If cond thn els) len = do
  compiledCond <- compile cond len
  compiledThn <- compile thn len
  compiledEls <- compile els len

  lift $ lift $ return $ If compiledCond compiledThn compiledEls

compile (TL.LetIn name expr exprIn) len = do
  compiledExpr <- compile expr len

  list <- get
  compiledExprIn <- lift $ fst <$> runStateT (compile exprIn (S len)) (Cons name list)

  lift $ lift $ return $ LetIn name compiledExpr compiledExprIn

compile TL.Skip _ = lift $ lift $ return Skip
