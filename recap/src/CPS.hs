module CPS () where
import Control.Monad (when)
import Data.Char (digitToInt, intToDigit)
import Control.Monad.Trans.Cont (ContT (..), runCont, callCC, shiftT, Cont, evalCont, resetT)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity (runIdentity))

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

addCps :: Int -> Int -> ((Int -> r) -> r)
addCps x y = \k -> k (add x y)

squareCps :: Int -> ((Int -> r) -> r)
squareCps x = \k -> k (square x)

pythagorasCps :: Int -> Int -> ((Int -> r) -> r)
pythagorasCps x y = \k ->
  squareCps x $ \x_squared ->
    squareCps y $ \y_squared ->
      addCps x_squared y_squared $ k

fun :: Int -> IO String
fun n = (`runContT` return) $ do
    str <- callCC $ \exit1 -> do                            -- define "exit1"
        when (n < 10) (exit1 (show n))
        lift $ print "b"
        return ""
        -- let ns = map digitToInt (show (n `div` 2))
        -- n' <- callCC $ \exit2 -> do                         -- define "exit2"
        --     when ((length ns) < 3) (exit2 (length ns))
        --     when ((length ns) < 5) (exit2 n)
        --     when ((length ns) < 7) $ do
        --         let ns' = map intToDigit (reverse ns)
        --         exit1 (dropWhile (=='0') ns')               --escape 2 levels
        --     return $ sum ns
        -- return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
    lift $ print "a"
    return $ "Answer: " ++ str

callCC' :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC' f = ContT $ \ c -> runContT (f (\ x -> ContT $ \ _ -> c x)) c

-- data Exception = Ex1 | Ex2
data Error r a = DivByZero (a -> Identity (Either (Error r a) r)) | Er

type MyCont r a = Cont (Either (Error r a) r) a

div' :: Int -> Int -> MyCont r Int
div' _ 0 = shiftT (return . Left . DivByZero)
div' x y = return $ x `div` y

add' :: Int -> Int -> MyCont r Int
add' x y = return $ x + y

mul' :: Int -> Int -> MyCont r Int
mul' x y = return $ x * y

fun' :: Int -> Int -> Either (Error Int Int) Int
fun' x y = evalCont $ resetT $ Right <$> (do
  x2 <- mul' x x
  y2 <- mul' y y
  a <- div' x2 y2
  add' a 1)

fun'' :: Either (Error Int Int) Int -> Int
fun'' x = case x of
  Left (DivByZero c) -> runIdentity $ fun'' <$> c 1
  Left Er -> 0
  Right y -> y

-- try :: ContT r m a -> (Exception -> ContT r m a) -> ContT r m a
-- try body throw = 

-- div :: Int -> Int -> ContT r m Exception -> ContT r m Int 
-- div _ 0 handler = undefined
-- div x y _ = undefined
