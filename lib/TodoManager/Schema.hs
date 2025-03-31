{-# LANGUAGE NoImplicitPrelude #-}

-- | sd
module TodoManager.Schema (createTableStatements) where

import Control.Applicative ((<$>))
import Data.List ((++))
import Data.String (String, fromString)
import Database.SQLite.Simple (Query)

createTodoTableString :: String
createTodoTableString =
  "CREATE TABLE IF NOT EXISTS Todo"
    ++ " (id INTEGER PRIMARY KEY, description TEXT NOT NULL UNIQUE, priority TEXT NOT NULL DEFAULT 'Medium', creation_time TEXT NOT NULL, last_update_time TEXT NOT NULL)"

createTodoDueTimeTableString :: String
createTodoDueTimeTableString =
  "CREATE TABLE IF NOT EXISTS TodoDueTime"
    ++ " (id INTEGER PRIMARY KEY, due_time TEXT NOT NULL, todo_id INTEGER NOT NULL UNIQUE REFERENCES Todo(id) ON UPDATE CASCADE ON DELETE CASCADE)"

createTodoCompletionTimeTableString :: String
createTodoCompletionTimeTableString =
  "CREATE TABLE IF NOT EXISTS TodoCompletionTime"
    ++ " (id INTEGER PRIMARY KEY, completion_time TEXT NOT NULL, todo_id INTEGER NOT NULL UNIQUE REFERENCES Todo(id) ON UPDATE CASCADE ON DELETE CASCADE)"

createTableStatements :: [Query]
createTableStatements = fromString <$> [createTodoTableString, createTodoDueTimeTableString, createTodoCompletionTimeTableString]
