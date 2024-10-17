module Lambda where

import Control.Monad.State

data Dsl
  = Var String
  | Lam String Dsl
  | App Dsl Dsl
  deriving (Show)

newtype DslState = DslState {getState :: [(String, Dsl)]}

eval :: Dsl -> Dsl
eval term = evalState (eval' term) (DslState [])

eval' :: Dsl -> State DslState Dsl
eval' (Var v) = do
  state <- get
  return $ case lookup v (getState state) of
    Just x -> x
    Nothing -> Var v
eval' (App body arg) = do
  body <- eval' body
  arg <- eval' arg
  case body of
    Lam v e -> do
      state <- get
      put $ DslState {getState = (v, arg) : getState state}
      eval' e
    t -> return $ App t arg
eval' (Lam v e) = Lam v <$> eval' e

-- pair = \f.\s.\b.b f s
pair = Lam "f" $ Lam "s" $ Lam "b" $ App (App (Var "b") (Var "f")) (Var "s")

-- frst = \p. p tru
frst = Lam "p" $ App (Var "p") tru

-- tru = \t.\f.t
tru = Lam "t" $ Lam "_f" $ Var "t"

fstOfPair = App frst (App (App pair (Var "u")) (Var "w"))

-- (\xy.x)y
boom = App (Lam "x" (Lam "y" $ Var "x")) (Var "y")