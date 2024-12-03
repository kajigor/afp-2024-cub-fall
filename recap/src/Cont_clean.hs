{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use const" #-}

module Cont_clean () where
import Control.Monad (when)
import Data.Char (digitToInt, intToDigit)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity (runIdentity))
-- Using the Cont monad from the transformers package.
import Control.Monad.Trans.Cont 
import Control.Monad.Identity (IdentityT(..))

addCont :: Int -> Int -> Cont r Int
addCont x y = return (x + y)

squareCont :: Int -> Cont r Int
squareCont x = return (x * x)

pythagorasCont :: Int -> Int -> Cont r Int
pythagorasCont x y = do
    x_squared <- squareCont x
    y_squared <- squareCont y
    addCont x_squared y_squared

runCps :: Cont r r -> r
runCps m = runCont m id

addCont' :: Int -> Int -> Cont String Int
addCont' x y = return (x + y)

squareCont' :: Int -> Cont String Int
squareCont' x = return (x * x)

pythagorasCont' :: Int -> Int -> Cont String Int
pythagorasCont' x y = do
    x_squared <- squareCont x
    y_squared <- squareCont y
    addCont x_squared y_squared

reset :: Cont r r -> r
reset x = runCont x id

shift' :: ((a -> r) -> r) -> Cont r a
shift' f = ContT (\k -> return $ f (runIdentity . k))

divCont' :: Int -> Int -> Cont String Int
divCont' x 0 = shift (\k -> return "Division by zero")
divCont' x y = return (x `div` y)

