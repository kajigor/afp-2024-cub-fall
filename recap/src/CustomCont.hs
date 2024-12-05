{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module CustomCont () where

newtype Cont r a = Cont {runCont :: (a -> r) -> r}

instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f x = Cont (\c -> runCont x (\y -> c (f y)))

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure x = Cont (\c -> c x)
  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  (<*>) x y = Cont (\c -> runCont x (\f -> runCont (f <$> y) c))

instance Monad (Cont r) where
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  (>>=) m f = Cont (\c -> runCont m (\x -> runCont (f x) c))

reset' :: Cont r r -> r
reset' x = runCont x id

shift' :: ((a -> r) -> r) -> Cont r a
shift' f = Cont (\c -> f c)

reset :: Cont r r -> Cont r' r
reset x = Cont $ \k -> k $ runCont x id

shift :: ((a -> r) -> Cont r r) -> Cont r a
shift f = Cont (\k -> runCont (f k) id)

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k
