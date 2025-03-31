{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | The core library of TodoManager
module TodoManager.Types.Internal
  ( Description (Description)
  , Priority (Low, Medium, High)
  , CreationTime (CreationTime)
  , DueTime (DueTime)
  , CompletionTime (CompletionTime)
  , LastUpdateTime (LastUpdateTime)
  , Todo
  , TodoCompletionTime
  , TodoCompletionTimeT (TodoCompletionTimeT, _todoCompletionTime, _todoCompletionTimeTodoId)
  , TodoDueTime
  , TodoDb (TodoDb)
  , TodoDueTimeT (TodoDueTimeT, _todoDueTime, _todoDueTimeTodoId)
  , TodoT (..)
  , PrimaryKey (TodoId)
  , makeCompletionTime
  , makeCreationTime
  , makeDescription
  , makeDueTime
  , makeLastUpdateTime
  , todoDb
  )
where

import Control.Applicative ((<$>))
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Text (Text, pack, unpack)
import Data.Time.LocalTime (ZonedTime)
import Database.Beam
  ( Beamable
  , Columnar
  , Database
  , DatabaseSettings
  , FromBackendRow (fromBackendRow)
  , Generic
  , HasSqlEqualityCheck
  , Identity
  , Table (PrimaryKey, primaryKey)
  , TableEntity
  , dbModification
  , defaultDbSettings
  , fieldNamed
  , modifyTableFields
  , setEntityName
  , tableModification
  , withDbModification
  )
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import Database.Beam.Sqlite.Connection (Sqlite)
import Prelude (Read, Show, read)

type Description :: Type
newtype Description = Description Text
  deriving newtype (Eq, HasSqlEqualityCheck Sqlite, Read, Show)

makeDescription :: Text -> Description
makeDescription = Description

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Description where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite Description where
  fromBackendRow = read . unpack <$> fromBackendRow

type Priority :: Type
data Priority = Low | Medium | High
  deriving stock (Eq, Read, Show)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Priority where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite Priority where
  fromBackendRow = read . unpack <$> fromBackendRow

type CreationTime :: Type
newtype CreationTime = CreationTime ZonedTime
  deriving newtype (Read, Show)

makeCreationTime :: ZonedTime -> CreationTime
makeCreationTime = CreationTime

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CreationTime where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite CreationTime where
  fromBackendRow = read . unpack <$> fromBackendRow

type LastUpdateTime :: Type
newtype LastUpdateTime = LastUpdateTime ZonedTime
  deriving newtype (Read, Show)

makeLastUpdateTime :: ZonedTime -> LastUpdateTime
makeLastUpdateTime = LastUpdateTime

instance HasSqlValueSyntax be String => HasSqlValueSyntax be LastUpdateTime where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite LastUpdateTime where
  fromBackendRow = read . unpack <$> fromBackendRow

type DueTime :: Type
newtype DueTime = DueTime ZonedTime
  deriving newtype (Read, Show)

makeDueTime :: ZonedTime -> DueTime
makeDueTime = DueTime

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DueTime where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite DueTime where
  fromBackendRow = read . unpack <$> fromBackendRow

type CompletionTime :: Type
newtype CompletionTime = CompletionTime ZonedTime
  deriving newtype (Read, Show)

makeCompletionTime :: ZonedTime -> CompletionTime
makeCompletionTime = CompletionTime

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CompletionTime where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite CompletionTime where
  fromBackendRow = read . unpack <$> fromBackendRow

type TodoT :: (Type -> Type) -> Type
type role TodoT nominal
data TodoT f
  = TodoT
  { _todoId :: Columnar f Int32
  , _todoDescription :: Columnar f Description
  , _todoPriority :: Columnar f Priority
  , _todoCreationTime :: Columnar f CreationTime
  , _todoLastUpdateTime :: Columnar f LastUpdateTime
  }
  deriving stock Generic
  deriving anyclass Beamable

type Todo :: Type
type Todo = TodoT Identity

instance Table TodoT where
  data PrimaryKey TodoT f = TodoId (Columnar f Int32)
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = TodoId . _todoId

type TodoDueTimeT :: (Type -> Type) -> Type
type role TodoDueTimeT nominal
data TodoDueTimeT f
  = TodoDueTimeT
  { _todoDueTimeId :: Columnar f Int32
  , _todoDueTime :: Columnar f DueTime
  , _todoDueTimeTodoId :: PrimaryKey TodoT f
  }
  deriving stock Generic
  deriving anyclass Beamable

type TodoDueTime :: Type
type TodoDueTime = TodoDueTimeT Identity

instance Table TodoDueTimeT where
  data PrimaryKey TodoDueTimeT f = TodoDueTimeId (Columnar f Int32)
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = TodoDueTimeId . _todoDueTimeId

type TodoCompletionTimeT :: (Type -> Type) -> Type
type role TodoCompletionTimeT nominal
data TodoCompletionTimeT f
  = TodoCompletionTimeT
  { _todoCompletionTimeId :: Columnar f Int32
  , _todoCompletionTime :: Columnar f CompletionTime
  , _todoCompletionTimeTodoId :: PrimaryKey TodoT f
  }
  deriving stock Generic
  deriving anyclass Beamable

type TodoCompletionTime :: Type
type TodoCompletionTime = TodoCompletionTimeT Identity

instance Table TodoCompletionTimeT where
  data PrimaryKey TodoCompletionTimeT f = TodoCompletionTimeId (Columnar f Int32)
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = TodoCompletionTimeId . _todoCompletionTimeId

type TodoDb :: (Type -> Type) -> Type
type role TodoDb representational
data TodoDb f
  = TodoDb
  { _todos :: f (TableEntity TodoT)
  , _todoDueTimes :: f (TableEntity TodoDueTimeT)
  , _todoCompletionTimes :: f (TableEntity TodoCompletionTimeT)
  }
  deriving stock Generic

instance Database be TodoDb

todoDb :: forall be. DatabaseSettings be TodoDb
todoDb =
  defaultDbSettings
    `withDbModification` dbModification
      { _todos = setEntityName (pack "Todo")
      , _todoDueTimes =
          setEntityName (pack "TodoDueTime")
            <> modifyTableFields
              tableModification
                { _todoDueTimeId = fieldNamed $ pack "id"
                , _todoDueTime = fieldNamed $ pack "due_time"
                , _todoDueTimeTodoId = TodoId . fieldNamed $ pack "todo_id"
                }
      , _todoCompletionTimes =
          setEntityName (pack "TodoCompletionTime")
            <> modifyTableFields
              tableModification
                { _todoCompletionTimeId = fieldNamed $ pack "id"
                , _todoCompletionTime = fieldNamed $ pack "completion_time"
                , _todoCompletionTimeTodoId = TodoId . fieldNamed $ pack "todo_id"
                }
      }
