module Configuration (loadConfigurationReturningConnection) where

import Control.Monad (forM_, void, (>=>))
import Database.SQLite.Simple (Connection, execute_, open)
import System.Directory.OsPath (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesPathExist, getXdgDirectory)
import System.IO (writeFile)
import System.OsPath (OsPath, decodeUtf, takeDirectory, unsafeEncodeUtf, (<.>), (</>))

import TodoManager.Schema (createTableStatements)

configDirectory :: IO OsPath
configDirectory = getXdgDirectory XdgConfig $ unsafeEncodeUtf "todo-manager"

configFile :: OsPath
configFile = unsafeEncodeUtf "data" <.> unsafeEncodeUtf "db"

configFilePath :: IO OsPath
configFilePath = do
  parentDirectory <- configDirectory
  pure $ parentDirectory </> configFile

-- [TODO]: Handle exceptions
openConnectionOnOsPath :: OsPath -> IO Connection
openConnectionOnOsPath = decodeUtf >=> open

createConfigurationReturningConnection :: OsPath -> IO Connection
createConfigurationReturningConnection configurationFilePath = do
  -- Create the configuration directory
  void
    $ createDirectoryIfMissing
      True -- Create parent directories
      (takeDirectory configurationFilePath)
  configurationFilePathAsString <- decodeUtf configurationFilePath
  writeFile configurationFilePathAsString ""
  connection <- open configurationFilePathAsString
  forM_ createTableStatements $ execute_ connection
  pure connection

loadConfigurationReturningConnection :: IO Connection
loadConfigurationReturningConnection = do
  configurationFilePath <- configFilePath
  configurationFileExists <- doesPathExist configurationFilePath
  if configurationFileExists
    then openConnectionOnOsPath configurationFilePath
    else createConfigurationReturningConnection configurationFilePath
