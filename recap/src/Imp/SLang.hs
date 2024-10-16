{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Imp.SLang where

import Control.Monad.State (StateT (runStateT), get, put)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Class (lift)
import qualified Imp.Lang as L
import qualified Imp.TLang as TL
import Text.Read (readMaybe, get)
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
  Seq ::
    SafeExpr t i -> SafeExpr t i -> SafeExpr t i
  If ::
    SafeExpr Bool i -> SafeExpr t i -> SafeExpr t i -> SafeExpr t i
  LetIn ::
    String -> SafeExpr Int i -> SafeExpr t (S i) -> SafeExpr t i
  Skip ::
    SafeExpr () i

data Error = ParsingErr String
  deriving (Eq, Show)

data SizedList v s where
  Nil :: SizedList v Z
  Cons :: v -> SizedList v s -> SizedList v (S s)

type EvalM i = StateT (SizedList Int i) (ExceptT Error IO)

defineAction :: Num t => Op -> (t -> t -> t)
defineAction op = case op of
  L.Plus -> (+)
  L.Minus -> (-)

evalExpr :: Show t => GEQ i ie -> SafeExpr t ie -> EvalM i t
evalExpr geq (Const x) = lift $ lift $ return x

evalExpr geqE (Var geqI iv) = do
  -- В данном случае geqI -- внутреннее доказательство того, что переменная
  -- с индексом iv ссылается на РЕАЛЬНОЕ значение в стеке из значений
  -- А geqE задает НОВЫЙ размер стека

  -- Проверяем, что мы не уменьшаем размер стека переменных
  let _ = geqTrans geqE geqI

  value <- Control.Monad.State.get

  case value of
    Nil -> lift $ throwE $ ParsingErr "Something went wrong. This variable does not exist"
    Cons v tail -> lift $ lift $ return v


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

  branchValue <- if condValue
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

compile :: (Monad m) => (Numm i) => TL.TExpr t -> i -> CompileM i m (SafeExpr t i)
compile = undefined
