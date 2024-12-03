{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Coroutines () where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad (replicateM_)
import qualified Control.Monad.Trans.Cont as C (shift, shiftT)
import Data.Bifunctor (first)

-- The CoroutineT monad is just ContT stacked with a StateT containing the suspended coroutines.
newtype CoroutineT r m a = CoroutineT {runCoroutineT' :: ContT r (StateT [CoroutineT r m r] m) a}
    deriving (Functor,Applicative,Monad,MonadCont,MonadIO)

instance MonadTrans (CoroutineT r) where
  lift :: Monad m => m a -> CoroutineT r m a
  lift = CoroutineT . lift . lift

evalCoroutineT :: (Monad m) => CoroutineT r m r -> m r
evalCoroutineT m = let x = runCoroutineT' m
                       y = evalContT x
                       z = runStateT y []
                       g = fst <$> z in g

-- resetT :: (Monad m) => CoroutineT r m r -> CoroutineT r' m r
-- resetT = lift . evalContT
-- {-# INLINE resetT #-}


shiftT' :: (Monad m) => ((a -> StateT [CoroutineT r m r] m r) -> CoroutineT r m r) -> CoroutineT r m a
shiftT' f = let x = runCoroutineT' . f
                y = C.shiftT x in CoroutineT y

-- aa :: (Monad m) => m r -> StateT [CoroutineT r m r] m r
-- aa = lift

-- bb :: (Monad m) => (a -> m r) -> (a -> StateT [CoroutineT r m r] m r)
-- bb f = lift . f


dd :: (Monad m) => (a -> StateT [CoroutineT r m r] m r) -> (a -> m r)
dd f = (\p -> fst <$> runStateT p []) . f

cc :: (Monad m) => ((a -> m r) -> CoroutineT r m r) -> ((a -> StateT [CoroutineT r m r] m r) -> CoroutineT r m r)
cc f = f . dd

shiftT :: (Monad m) => ((a -> m r) -> CoroutineT r m r) -> CoroutineT r m a
shiftT f = shiftT' $ cc f

-- shiftT :: (Monad m) => ((a -> m r) -> CoroutineT r m r) -> CoroutineT r m a
-- shiftT f = CoroutineT $ ContT $ \k -> let x = \p -> fst <$> (runStateT p [])
--                                           y = x . k
--                                           z = f y
--                                           g = evalCoroutineT z
--                                           l = lift g in l
-- shiftT f = CoroutineT $ C.shiftT (\g -> let h = f (\x -> return x) in undefined)

-- Used to manipulate the coroutine queue.
getCCs :: Monad m => CoroutineT r m [CoroutineT r m r]
getCCs = CoroutineT $ lift get

putCCs :: Monad m => [CoroutineT r m r] -> CoroutineT r m ()
putCCs = CoroutineT . lift . put

-- Pop and push coroutines to the queue.
dequeue :: Monad m => CoroutineT r m r
dequeue = do
    current_ccs <- getCCs
    case current_ccs of
        [] -> return undefined
        (p:ps) -> do
            putCCs ps
            p

queue :: Monad m => CoroutineT r m r -> CoroutineT r m ()
queue p = do
    ccs <- getCCs
    putCCs (ccs++[p])

-- The interface.
yield :: Monad m => CoroutineT r m ()
yield = shiftT $ \k -> do
    queue (lift $ k ())
    dequeue

fork :: Monad m => CoroutineT r m () -> CoroutineT r m ()
fork p = shiftT $ \k -> do
    queue (lift $ k ())
    p
    dequeue

-- Exhaust passes control to suspended coroutines repeatedly until there isn't any left.
exhaust :: Monad m => CoroutineT r m ()
exhaust = do
    exhausted <- null <$> getCCs
    if not exhausted
        then yield >> exhaust
        else return ()

-- Runs the coroutines in the base monad.
runCoroutineT :: Monad m => CoroutineT r m r -> m r
runCoroutineT = flip evalStateT [] . flip runContT return . runCoroutineT' . (<* exhaust)

printOne :: (MonadIO m, Show a) => a -> CoroutineT r m ()
printOne n = do
    liftIO (print n)
    yield

example :: IO ()
example = runCoroutineT $ do
    fork $ replicateM_ 3 (printOne 3)
    fork $ replicateM_ 4 (printOne 4)
    replicateM_ 2 (printOne 2)
