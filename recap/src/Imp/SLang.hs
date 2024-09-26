{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Imp.SLang where

import Control.Monad.State (StateT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Imp.Lang as L
import qualified Imp.TLang as TL
import Text.Read (readMaybe)

data Z = Z

data S a = S a

data GEQ a b where
  GEQZ :: GEQ a Z
  GEQS :: GEQ a b -> GEQ (S a) (S b)

geqTrans :: GEQ a b -> GEQ b c -> GEQ a c
geqTrans _ GEQZ = GEQZ
geqTrans (GEQS ab) (GEQS cd) = GEQS (geqTrans ab cd)

class Numm a where
  geqRefl :: GEQ a a

instance Numm Z where
  geqRefl = GEQZ

instance (Numm a) => Numm (S a) where
  geqRefl = GEQS geqRefl

data SafeExpr t i where
  Var ::
    GEQ i (S iv) ->
    iv ->
    SafeExpr Int i
  Const :: Int -> SafeExpr Int Z
  BinOp :: L.Op -> SafeExpr Int i -> SafeExpr Int i -> SafeExpr Int i
  Read :: SafeExpr Int Z
  Write :: SafeExpr Int i -> SafeExpr () i
  Seq :: SafeExpr () i -> SafeExpr a  -> SafeExpr a
  If :: SafeExpr Int -> SafeExpr a -> SafeExpr a -> SafeExpr a
  LetIn :: String -> SafeExpr Int -> SafeExpr a -> SafeExpr a
  Skip :: SafeExpr ()

data Error = ParsingErr String
  deriving (Eq, Show)

data SizedList v s where
  Nil :: SizedList v Z
  Cons :: v -> SizedList v s -> SizedList v (S s)

type EvalM i = StateT (SizedList Int i) (ExceptT Error IO)

evalExpr :: GEQ i ie -> SafeExpr t ie -> EvalM i t
evalExpr = undefined

data SafeCompileError = UndefinedVar String
  deriving (Eq, Show)

type CompileM i m = StateT (SizedList String i) (ExceptT SafeCompileError m)

compile :: (Monad m) => (Numm i) => TL.TExpr t -> i -> CompileM i m (SafeExpr t i)
compile = undefined
