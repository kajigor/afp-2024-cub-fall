{-# LANGUAGE DeriveFunctor #-}

module DslFree.Lang where

import Control.Monad.Free
import MyMonadTransformers.Lang (BinOp (..), UnOp (..))

type StepItem = Free StepDslFunctor Float

data StepDslFunctor next 
    = Bin BinOp next 
    | Un UnOp next 
    | Store String next
    | Recall String next
    | Put Float next 
    deriving (Functor)

