module Main where

import Configuration (loadConfigurationReturningConnection)

main :: IO ()
main = do
  connection <- loadConfigurationReturningConnection
  putStrLn "Welcome to my TODO List Manager!"
  loop

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
