module Cps2 () where

import Control.Monad.Cont (ContT, MonadCont (callCC), evalContT)
import Control.Monad.Trans (MonadIO (liftIO), lift)
import Control.Monad.Trans.Cont (reset, resetT, shiftT)

type Re r a = ContT r IO a

quux :: Re r Int
quux = callCC $ \k -> do
  let n = 5
  _ <- k n
  lift $ print "a"
  return 1

go :: IO ()
go =
  evalContT
    ( do
        x <- quux
        lift $ print (show x)
    )

quux1 :: Re r Int
quux1 = resetT $ do
  let n = (5 :: Integer)
  g <- shiftT (\k -> do
    
    return 9
    lift $ print "b"
    lift $ k 5
    lift $ k 6
    return 5
    )
  lift $ print $ "a"
  return 1

go22 :: IO ()
go22 =
  evalContT
    ( do
        x <- quux1
        lift $ print (show x)
    )