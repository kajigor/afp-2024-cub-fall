module Main (main) where

import qualified Intro.Initial as II 
import qualified Intro.FinalBad as IFB
import qualified Intro.Final as IF 
import qualified ExpressionProblem.InitialMul as EI
import qualified ExpressionProblem.InitialWithOld as EO

main :: IO ()
main = do 
  -- II.main
  -- IFB.main
  -- IF.main
  EI.main
  EO.main 
