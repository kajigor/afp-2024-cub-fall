{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Monad.Writer
import Control.Monad.Free(Free(..), foldFree)
import Text.Printf (printf)
import Data.List (intercalate)

type QueryMonad a = Free QueryF a
type Query = QueryMonad ()   

data QueryF a where
    Select :: String -> [String] -> QueryMonad a -> a -> QueryF a -- SELECT [column1, c] FROM Customers;
    Insert :: String -> [String] -> [String] -> (QueryMonad a) -> a -> QueryF a  -- INSERT INTO table_name (column1) VALUES (value1);
    Update :: String -> [String] -> [String] -> (QueryMonad a) -> a -> QueryF a -- UPDATE table_name SET column1 = value1;
    Delete :: String -> (QueryMonad a) -> a -> QueryF a -- DELETE FROM table_name;
    Where :: String -> Operation -> String -> a -> QueryF a -- WHERE column1 > value1;
    WhereTrue :: a -> QueryF a
    OrderBy :: [String] -> a -> QueryF a  -- ORDER BY order_time, order_id
    Limit :: Int -> a -> QueryF a  -- Limit 10
    deriving (Functor)

data Operation = Equal | GreaterThan | LessThan | GreaterThanOrEqual | LessThanOrEqual | NotEqual

instance Show Operation where
    show Equal  = "="
    show NotEqual = "!="
    show GreaterThan  = ">"
    show LessThan  = "<"
    show GreaterThanOrEqual = ">="
    show LessThanOrEqual = "<="


select :: String -> [String] -> Query -> Query
select table columns body = Free $ Pure <$> Select table columns body ()

insert ::  String -> [String] -> [String] -> Query -> Query
insert table col value body = Free $ Pure <$> Insert table col value body ()

update :: String -> [String] -> [String] -> Query -> Query
update table col value body = Free $ Pure <$> Update table col value body ()

delete :: String -> Query -> Query
delete table body =  Free $ Pure <$> Delete table body ()

whereClause :: String -> Operation -> String -> Query 
whereClause col op value = Free $ Pure <$> Where col op value ()

whereTrue :: Query 
whereTrue = Free $ Pure <$> WhereTrue ()

orderBy :: [String] -> Query
orderBy columns = Free $ Pure <$> OrderBy columns ()

limit :: Int -> Query
limit lim =  Free $ Pure <$> Limit lim ()

render' ::  QueryMonad () -> IO ()
render' = foldFree renderQuery'


quoteColumns :: [String] -> String
quoteColumns columns = intercalate ", " (map (\col -> "\"" ++ col ++ "\"") columns)

renderQuery' :: QueryF a -> IO a
renderQuery' (Select table columns body next) = do
    printf "\nSELECT \"%s\" FROM %s" (quoteColumns columns) table
    _ <- foldFree renderQuery' body
    printf ";"
    return next
renderQuery' (Insert table columns values body next) = do
    printf "\nINSERT INTO \"%s\" (%s) VALUES (%s)" table (quoteColumns columns) (quoteColumns values)
    _ <- foldFree renderQuery' body
    printf ";"
    return next
renderQuery' (Update table columns values body next) = do
    let setClause = intercalate ", " (zipWith (\col val -> col ++ " = " ++ val) columns values)
    printf "\nUPDATE \"%s\" SET %s" table setClause
    _ <- foldFree renderQuery' body
    printf ";"
    return next
renderQuery' (Delete table body next) = do
    printf "\nDELETE FROM \"%s\"" table
    _ <- foldFree renderQuery' body
    return next
renderQuery' (Where col op value next) = do
    let opStr = show op
    printf "\nWHERE %s %s %s" col opStr value
    return next
renderQuery' (WhereTrue next) = do
    printf "\nWhere True"
    return next
renderQuery' (OrderBy columns next) = do
    printf "\nORDER BY %s" (quoteColumns columns)
    return next
renderQuery' (Limit lim next) = do
    printf "\nLIMIT %d" lim
    return next


queryComponent :: Query
queryComponent = do
    insert "table" ["col1", "col2"] ["val1", "val2"] $ do
        whereClause "a " Equal "b"
    update "table" ["col1", "col2"] ["1", "3"] $ do
        whereClause "l" LessThan "k"
    select "table" ["col1", "col2", "col3"] $ do
        whereTrue
        orderBy ["col1", "col2"]
        limit 100

main :: IO ()
main = do render' queryComponent
