{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cps () where

-- Result of computation in CPS
-- (a -> r) is the continuation
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

{-# HLINT ignore "Avoid lambda" #-}
instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f x = Cont (\c -> runCont x (\y -> c (f y)))

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure x = Cont (\c -> c x)
  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  (<*>) x y = Cont (\c -> runCont x (\f -> runCont (f <$> y) c))
  -- (<*>) x y = runCont x (\f -> f <$> y)

instance Monad (Cont r) where
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  (>>=) m f = Cont (\c -> runCont m (\x -> runCont (f x) c))

reset :: Cont r r -> r
reset x = runCont x id

shift :: ((a -> r) -> r) -> Cont r a
shift f = Cont (\c -> f c)

reset' :: Cont r r -> Cont r' r
reset' x = Cont $ \k -> k $ runCont x id

shift' :: ((a -> r) -> Cont r r) -> Cont r a
shift' f = Cont (\k -> runCont (f k) id)

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k


data Error a r = DivByZero (a -> Either (Error a r) r) | AnotherError (a -> Either (Error a r) r)
newtype ErrorOr r a = ErrorOr { errorOr :: Cont (Either (Error a r) r) a }

div' :: Int -> Int -> ErrorOr r Int
div' a b = if b == 0 then ErrorOr $ shift (\c -> Left $ DivByZero c) else ErrorOr $ return $ div a b

aa :: Either (Error Int Int) Int
aa = reset $ do
  x <- errorOr $ div' 2 0
  y <- return $ x + 5
  return $ Right y

bb :: Either (Error Int Int) Int
bb = reset $ do
  x <- errorOr $ div' 2 1
  return $ Right x

cc :: Either (Error Int Int) Int -> String
cc f = case f of
  Left (DivByZero c) -> cc $ c 1
  Right x -> show x
  _ -> undefined
