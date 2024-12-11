{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import SQLSyntax
import Database.SQLite.Simple.FromRow

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

-- To try it, run the following command into terminal
-- sqlite3 test.db "CREATE TABLE test (id INTEGER PRIMARY KEY, str text); INSERT INTO test (str) VALUES ('test string');
main = do
    runQuery "test.db" (createInsertQuery "INSERT INTO test (str) VALUES (?)" (OneEl ("ABRA CADABRA !!!" :: String)))
    runQuery "test.db" (createSelectQuery "SELECT * from test" :: SQLCommand [TestField])
