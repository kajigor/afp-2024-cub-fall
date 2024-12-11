module Main (main) where

import Lib
import MyMonadTransformers.Lang 
import MyMonadTransformers.Eval 
import DslFree.SmartConstructors
import DslFree.Eval


prg :: Step 
prg = Seq (Put 1) (Seq (Put 2) (Bin Plus))

prg0 :: Step 
prg0 = Seq (Put 1) $ Seq (Put 2) $ Seq (Store "a") $ Seq (Un Sin) $ Store "b"

prg1 :: Step 
prg1 = Seq (Put 1) $ Seq (Put 2) $ Seq (Store "a") $ Seq (Bin Mult) $ Seq (Store "b") $ Seq (Recall "a") $ Seq (Recall "b") $ Bin Plus

prg2 :: Step 
prg2 = Store "a"

prg_ = do 
  put_ 1 
  put_ 2 
  mplus_

prg0_ = do 
  put_ 1 
  put_ 2 
  store_ "a"
  msin_
  store_ "b"

prg1_ = do 
  put_ 1 
  put_ 2 
  store_ "a"
  mmult_
  store_ "b"
  recall_ "a"
  recall_ "b" 
  mplus_ 


main :: IO ()
main = do 
  mapM_ runProgram [prg, prg0, prg1, prg2]
  runCalc prg_
  runCalc prg0_
  runCalc prg1_
