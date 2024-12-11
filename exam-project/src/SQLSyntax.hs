{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module SQLSyntax (createInsertQuery, createSelectQuery, InsertingItem(..), SQLCommand, runQuery) where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromRow
import Control.Monad.Trans.Maybe (MaybeT)


-- User API to convinient create tuples
data InsertingItem where 
    OneEl :: ToField a => a -> InsertingItem
    TwoEl :: (ToField a, ToField b) => a -> b -> InsertingItem
    ThreeEl :: (ToField a, ToField b, ToField c) => a -> b -> c -> InsertingItem


data SQLCommand a where 
    Insert :: Query -> InsertingItem -> SQLCommand ()
    Select :: FromRow a => Query -> SQLCommand [a]


-- smart constructors --
createInsertQuery query item = Insert query item
createSelectQuery query = Select query


-- inner logic --
executeCommand :: (Show a) => Connection -> SQLCommand a -> IO a
executeCommand conn (Insert query item) = 
    case item of
        OneEl el -> execute conn query (Only el)
        TwoEl el1 el2 -> execute conn query (el1, el2)
        ThreeEl el1 el2 el3 -> execute conn query (el1, el2, el3)


executeCommand conn (Select query) = 
    query_ conn query


-- api, works both for insert query and select --
runQuery :: (Show a) => String -> SQLCommand a -> IO a
runQuery dbName command = do
    conn <- open dbName

    result <- executeCommand conn command

    close conn

    return result
