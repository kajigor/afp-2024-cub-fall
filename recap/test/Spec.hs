import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (evalStateT, runStateT)
import Imp.Exprs
import Imp.Lang as L
import Imp.SLang as SL
import Imp.TLang as TL

main :: IO ()
main = do
  texpr <- runExceptT $ TL.compile expr4
  case texpr of
    Left err -> print err
    Right (TL.IntExpr ie) -> do
      sexpr <- runExceptT $ evalStateT (SL.compile ie (SL.S SL.Z)) (SL.Cons "y" SL.Nil)
      case sexpr of
        Left err -> print err
        Right se -> do
          res <- runExceptT $ evalStateT (SL.evalExpr (GEQS GEQZ) se) (SL.Cons 5 SL.Nil)
          print res
    Right (TL.UnitExpr ue) -> do
      sexpr <- runExceptT $ evalStateT (SL.compile ue (SL.S SL.Z)) (SL.Cons "y" SL.Nil)
      case sexpr of
        Left err -> print err
        Right se -> do
          res <- runExceptT $ evalStateT (SL.evalExpr (GEQS GEQZ) se) (SL.Cons 5 SL.Nil)
          print res
