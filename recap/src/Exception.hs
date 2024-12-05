module Exception () where

import Control.Monad.Cont (Cont)
import Control.Monad.Trans.Cont (evalCont, shiftT)
import Data.Functor.Identity (Identity (runIdentity))

data Error r a = DivByZero (a -> Identity (Either (Error r a) r)) | NegativeSqrt

type MyCont r a = Cont (Either (Error r a) r) a

div' :: Int -> Int -> MyCont r Int
div' _ 0 = shiftT (return . Left . DivByZero)
div' x y = return $ x `div` y

add :: Int -> Int -> MyCont r Int
add x y = return $ x + y

mul :: Int -> Int -> MyCont r Int
mul x y = return $ x * y

fixErrors :: Either (Error Int Int) Int -> Int
fixErrors x = case x of
  Left (DivByZero c) -> runIdentity $ fixErrors <$> c 1
  Left NegativeSqrt -> 0
  Right y -> y

example :: Int -> Int -> Either (Error Int Int) Int
example x y =
  evalCont $
    Right
      <$> ( do
              x2 <- mul x x
              y2 <- mul y y
              a <- div' x2 y2
              add a 1
          )
