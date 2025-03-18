{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Monad (void)
import Database.Beam.Sqlite (runBeamSqlite)
import Database.SQLite.Simple (Connection)
import Prelude (Bool (False, True), IO, Maybe, String, pure, putStrLn, ($), (++))

import Configuration (loadConfigurationReturningConnection)
import TodoManager.Queries (insertTodo)
import TodoManager.Types (CreationTime, Description, DueTime, Priority)

main :: IO ()
main = do
  connection <- loadConfigurationReturningConnection
  putStrLn "Welcome to my TODO List Manager!"
  loop

createTodo :: Connection -> Description -> Priority -> CreationTime -> Maybe DueTime -> IO ()
createTodo connection description priority creationTime maybeDueTime =
  void $ runBeamSqlite connection $ insertTodo description priority creationTime maybeDueTime

loop :: IO ()
loop = do
  pure ()

{-
    putStr "Enter command: "
    hFlush stdout
    input <- getLine
    putStrLn "Done!"
-}

{-
isLooping <- handleInput input
if isLooping
  then loop
  else return ()
-}

handleInput :: String -> IO Bool
handleInput "exit" = do
  putStrLn "Goodbye!"
  pure False
handleInput input = do
  putStrLn $ "You entered: " ++ input
  pure True
