-- | sd
module SchemaDefinition (createSchema) where

import Control.Monad (forM_)
import Data.String (fromString)
import Database.SQLite.Simple (Connection, Query, execute_)

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

createSchema :: Connection -> IO ()
createSchema connection = do
  forM_ createTableStatements $ execute_ connection
