{-# LANGUAGE DeriveFunctor #-}

module Query (
    QueryF,
    field,
    Query.or,
    Query.and,
    eq,
    gt,
    lt,
    Query.not,
    match,
    number,
    literal,
    toJSON
) where

import Control.Monad.Free
import Control.Monad.Writer

-- TODO: Support more literals
type Value = Either String Int

data Query a =
    Field String Pred a
    | Or QueryF a
    | And QueryF a
    deriving Functor

data Pred =
    Eq Value
    | Gt Value
    | Lt Value
    | Not Pred
    | Match QueryF

type QueryF = Free Query ()

field :: String -> Pred -> QueryF
field name p = liftF $ Field name p ()

or :: QueryF -> QueryF
or queries = liftF $ Or queries ()

and :: QueryF -> QueryF
and queries = liftF $ And queries ()

eq :: Value -> Pred
eq = Eq

gt :: Value -> Pred
gt = Gt

lt :: Value -> Pred
lt = Lt

not :: Pred -> Pred
not = Not

match :: QueryF -> Pred
match = Match

number :: Int -> Value
number = Right

literal :: String -> Value
literal = Left

toJSON :: QueryF -> String
toJSON q = execWriter $ toWriter q
    where
        toWriter :: QueryF -> Writer String ()
        toWriter qs = tell "{ " >> items qs >> tell " }"

        transform :: Query a -> Writer String a
        transform (Field name p a) = tell ("\"" ++ name ++ "\": ") >> transformPred p >> return a
        transform (Or queries a) = tell "\"$or\" : [ " >> items queries >> tell " ]" >> return a
        transform (And queries a) = tell "\"$and\" : [ " >> items queries >> tell " ]" >> return a

        transformPred :: Pred -> Writer String ()
        transformPred (Eq value) = tell $ valueToJSON value
        transformPred (Gt value) = tell $ "{ \"$gt\": " ++ valueToJSON value ++ " }"
        transformPred (Lt value) = tell $ "{ \"$lt\": " ++ valueToJSON value ++ " }"
        transformPred (Not p) = tell "{ \"$not\": " >> transformPred p >> tell " }"
        transformPred (Match query) = toWriter query

        items :: QueryF -> Writer String ()
        items = foldFree (\x -> transform x <* tell ", ")

        valueToJSON :: Value -> String
        valueToJSON (Left s) = "\"" ++ s ++ "\""
        valueToJSON (Right i) = show i
