{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad.Free(Free(..), foldFree)
import Text.Printf (printf)
import Data.List (intercalate)

type QueryMonad a = Free QueryF a
type Query = QueryMonad () 

data QueryF a where
    Select :: String -> [String] -> QueryMonad a -> a -> QueryF a -- SELECT [column1, c] FROM Customers;
    Insert :: ToSqlStr b => String -> [String] -> [b] -> (QueryMonad a) -> a -> QueryF a  -- INSERT INTO table_name (column1) VALUES (value1);
    Update :: ToSqlStr b => String -> [String] -> [b] -> (QueryMonad a) -> a -> QueryF a -- UPDATE table_name SET column1 = value1;
    Delete :: String -> (QueryMonad a) -> a -> QueryF a -- DELETE FROM table_name;
    Where :: String -> Operation -> String -> a -> QueryF a -- WHERE column1 > value1;
    WhereTrue :: a -> QueryF a
    OrderBy :: [String] -> a -> QueryF a  -- ORDER BY order_time, order_id
    Limit :: Int -> a -> QueryF a  -- Limit 10
    DropTable :: String -> a -> QueryF a -- DROP TABLE Customers;

class ToSqlStr b where
    toSqlString :: b -> String

instance ToSqlStr Bool where
    toSqlString b = if b then "TRUE" else "FALSE"

instance ToSqlStr Integer where
    toSqlString = show  

instance ToSqlStr Double where
    toSqlString = show  

instance ToSqlStr String where
    toSqlString b = "'" ++ b ++ "'"

instance Functor QueryF where
    fmap f (Select table cols qm a) = Select table cols (fmap f qm) (f a)
    fmap f (Insert table cols values qm a) = Insert table cols values (fmap f qm) (f a)
    fmap f (Update table cols values qm a) = Update table cols values (fmap f qm) (f a)
    fmap f (Delete table qm a) = Delete table (fmap f qm) (f a)
    fmap f (Where col op value a) = Where col op value (f a)
    fmap f (WhereTrue a) = WhereTrue (f a)
    fmap f (OrderBy cols a) = OrderBy cols (f a)
    fmap f (Limit n a) = Limit n (f a)
    fmap f (DropTable table a) = DropTable table (f a)

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

insert :: ToSqlStr b => String -> [String] -> [b] -> Query -> Query
insert table col value body = Free $ Pure <$> Insert table col value body ()

update :: ToSqlStr b => String -> [String] -> [b] -> Query -> Query
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
limit lim = Free $ Pure <$> Limit lim ()

dropTable :: String -> Query
dropTable tableName = Free $ Pure <$> DropTable tableName ()

render' ::  QueryMonad () -> IO ()
render' = foldFree renderQuery'

quoteColumns :: [String] -> String
quoteColumns columns = intercalate ", " (map (\col -> "\"" ++ col ++ "\"") columns)

renderQuery' :: QueryF a -> IO a
renderQuery' (Select table columns body next) = do
    printf "\nSELECT %s FROM \"%s\"" (quoteColumns columns) table
    _ <- foldFree renderQuery' body
    printf ";"
    return next
renderQuery' (Insert table columns values body next) = do
    printf "\nINSERT INTO \"%s\" (%s) VALUES (%s)" table (quoteColumns columns) (intercalate ", " $ map toSqlString values)
    _ <- foldFree renderQuery' body
    printf ";"
    return next
renderQuery' (Update table columns values body next) = do
    let setClause = intercalate ", " (zipWith (\col val -> col ++ " = " ++ val) columns (map toSqlString values))
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
    printf "\nWhere TRUE"
    return next
renderQuery' (OrderBy columns next) = do
    printf "\nORDER BY %s" (quoteColumns columns)
    return next
renderQuery' (Limit lim next) = do
    printf "\nLIMIT %d" lim
    return next
renderQuery' (DropTable tableName next) = printf "\nDROP TABLE %s;" tableName >> return next

queryComponent :: Query
queryComponent = do
    insert "table" ["col1", "col2"] ["val1", "val2"] $ do
        whereClause "a " Equal "b"
    update "table" ["col1", "col2"] [True, False] $ do
        whereClause "l" LessThan "k"
    select "table" ["col1", "col2", "col3"] $ do
        whereTrue
        orderBy ["col1", "col2"]
        limit 100
    dropTable "Customers"
    insert "table3" ["col1", "col2"]  ([1, 2] :: [Integer]) $ do
        whereTrue
    insert "table3" ["col1", "col2", "col3"]  ([1.0, 2.0, 3.0] :: [Double]) $ do
        whereTrue

main :: IO ()
main = do render' queryComponent