{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import SQLSyntax
import Database.SQLite.Simple.FromRow

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field


main = do
    runQuery "test.db" (createInsertQuery "INSERT INTO test (str) VALUES (?)" (OneEl ("ABRA CADABRA !!!" :: String)))
    runQuery "test.db" (createSelectQuery "SELECT * from test" :: SQLCommand [TestField])
