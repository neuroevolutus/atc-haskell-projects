{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Exception (bracket)
import Data.Foldable (forM_)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Text (pack)
import Data.Time.Clock (nominalDay)
import Data.Time.LocalTime (ZonedTime (zonedTimeToLocalTime), addLocalTime, getZonedTime)
import Data.Traversable (sequence)
import Database.Beam.Sqlite.Connection (runBeamSqlite)
import Database.SQLite.Simple (Connection, close, execute_, open, withConnection)
import Test.Hspec (around, aroundAll, describe, example, it, shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import Prelude (IO, String, pure, ($), (.), (<$>), (>), (>>=))

import TodoManager.Queries
  ( deleteTodo
  , insertTodo
  , selectAllTodos
  , setCompletionTime
  , setDueTime
  , unsetCompletionTime
  , unsetDueTime
  )
import TodoManager.Schema (createTableStatements)
import TodoManager.Types (CreationTime, Description, DueTime, Priority (Low))
import TodoManager.Types.Constructors (makeCompletionTime, makeCreationTime, makeDescription, makeDueTime)
import TodoManager.Types.Getters
  ( completionTimeToUtc
  , creationTimeToUtc
  , dueTimeToUtc
  , dueTimeToZonedTime
  , lastUpdateTimeToUtc
  )

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = testGroup "Tests" <$> sequence [unitTests]

inMemoryConnection :: String
inMemoryConnection = ":memory:"

withInMemoryConnection :: (Connection -> IO a) -> IO a
withInMemoryConnection = withConnection inMemoryConnection

standUpDatabaseOnConnection :: Connection -> IO ()
standUpDatabaseOnConnection = forM_ createTableStatements . execute_

withStoodUpDatabase :: (Connection -> IO a) -> IO a
withStoodUpDatabase =
  bracket
    do connection <- open inMemoryConnection; standUpDatabaseOnConnection connection; pure connection
    close

cleanGarageDescription :: Description
cleanGarageDescription = makeDescription . pack $ "Clean garage"

lowPriority :: Priority
lowPriority = Low

makeDueTimeOneDayFromNow :: IO DueTime
makeDueTimeOneDayFromNow =
  makeDueTime
    . (\zonedTime -> zonedTime{zonedTimeToLocalTime = addLocalTime nominalDay (zonedTimeToLocalTime zonedTime)})
    <$> getZonedTime

unitTests :: IO TestTree
unitTests = testSpec "Unit Tests" do
  around withInMemoryConnection $ describe "Todo Database" do
    it "should be instantiable" $ \connection ->
      example
        $ standUpDatabaseOnConnection connection
  aroundAll withStoodUpDatabase $ describe "Todo" do
    it "should be instantiable with due time" $ \connection -> example do
      creationTime <- makeCreationTime <$> getZonedTime :: IO CreationTime
      dueTime <- makeDueTimeOneDayFromNow
      runBeamSqlite connection $ insertTodo cleanGarageDescription lowPriority creationTime (Just dueTime)
    it "should be retrievable" $ \connection -> do
      [(description_, priority_, creationTime_, lastUpdateTime_, Just dueTime_, _)] <- runBeamSqlite connection selectAllTodos
      description_ `shouldBe` cleanGarageDescription
      priority_ `shouldBe` lowPriority
      let dueTimeUtc = dueTimeToUtc dueTime_
      dueTimeUtc `shouldSatisfy` (> creationTimeToUtc creationTime_)
      dueTimeUtc `shouldSatisfy` (> lastUpdateTimeToUtc lastUpdateTime_)
    it "should be able to have its due time changed" $ \connection -> do
      [(description_, _, _, _, Just dueTime_, _)] <- runBeamSqlite connection selectAllTodos
      let zonedTime = dueTimeToZonedTime dueTime_
      let newZonedTime = zonedTime{zonedTimeToLocalTime = addLocalTime nominalDay (zonedTimeToLocalTime zonedTime)}
      let newDueTime = makeDueTime newZonedTime
      let newDueTimeUtc = dueTimeToUtc newDueTime
      runBeamSqlite connection $ setDueTime description_ newDueTime
      [(_, _, _, _, Just newDueTime_, _)] <- runBeamSqlite connection selectAllTodos
      dueTimeToUtc newDueTime_ `shouldBe` newDueTimeUtc
    it "should be able to be marked completed" $ \connection -> do
      completionTime <- makeCompletionTime <$> getZonedTime
      let completionTimeUtc = completionTimeToUtc completionTime
      runBeamSqlite connection $ setCompletionTime cleanGarageDescription completionTime
      [(_, _, _, _, _, Just completionTime_)] <- runBeamSqlite connection selectAllTodos
      completionTimeUtc `shouldBe` completionTimeToUtc completionTime_
    it "should be able to be marked incomplete" $ \connection -> do
      runBeamSqlite connection $ unsetCompletionTime cleanGarageDescription
      [(_, _, _, _, _, Nothing)] <- runBeamSqlite connection selectAllTodos
      pure ()
    it "should be able to be marked as having no due time" $ \connection -> do
      runBeamSqlite connection $ unsetDueTime cleanGarageDescription
      [(_, _, _, _, Nothing, _)] <- runBeamSqlite connection selectAllTodos
      pure ()
    it "should be removable" $ \connection -> do
      runBeamSqlite connection $ deleteTodo cleanGarageDescription
      [] <- runBeamSqlite connection selectAllTodos
      pure ()
    it "should be instantiable without due time" $ \connection -> example do
      creationTime <- makeCreationTime <$> getZonedTime :: IO CreationTime
      runBeamSqlite connection $ insertTodo cleanGarageDescription lowPriority creationTime Nothing
      [(_, _, _, _, Nothing, _)] <- runBeamSqlite connection selectAllTodos
      pure ()
    it "should be able to have a due time added even if not instantiated with one" $ \connection -> do
      dueTime <- makeDueTimeOneDayFromNow
      let dueTimeUtc = dueTimeToUtc dueTime
      runBeamSqlite connection $ setDueTime cleanGarageDescription dueTime
      [(_, _, _, _, Just dueTime_, _)] <- runBeamSqlite connection selectAllTodos
      dueTimeUtc `shouldBe` dueTimeToUtc dueTime_
