-- | The core library of TodoManager
module TodoManager
  ( createTodo
  , createDescription
  , createCreationTime
  , createDueTime
  , createCompletionTime
  , setDescription
  , setDueTime
  , markComplete
  , Description
  , Priority
  , CreationTime
  , DueTime
  , CompletionTime
  )
where

import Data.Int (Int32)
import Data.Text (Text, unpack)
import Data.Time.LocalTime (LocalTime)
import Database.Beam
  ( Beamable
  , Columnar
  , Database
  , DatabaseSettings
  , FromBackendRow (fromBackendRow)
  , Generic
  , Identity
  , LensFor (LensFor)
  , Table (PrimaryKey, primaryKey)
  , TableEntity
  )
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax (sqlValueSyntax))
import Database.Beam.Sqlite.Connection (Sqlite)
import Database.SQLite.Simple (Connection)
import Lens.Micro ((^.))

newtype Description = Description Text
  deriving newtype (Read, Show)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Description where
  -- [TODO]: How is `autoSqlValueSyntax` accessible without explicit import?
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite Description where
  fromBackendRow = read . unpack <$> fromBackendRow

data Priority = Low | Medium | High
  deriving (Read, Show)

newtype CreationTime = CreationTime LocalTime
  deriving newtype (Read, Show)

newtype LastUpdateTime = LastUpdateTime LocalTime
  deriving newtype (Read, Show)

newtype DueTime = DueTime LocalTime
  deriving newtype (Read, Show)

newtype CompletionTime = CompletionTime LocalTime
  deriving newtype (Read, Show)

data TodoT f
  = Todo
  { _todoId :: Columnar f Int32
  , _todoDescription :: Columnar f Description
  , _todoPriority :: Columnar f Priority
  , _todoCreationTime :: Columnar f CreationTime
  , _todoLastUpdateTime :: Columnar f LastUpdateTime
  }
  deriving (Beamable, Eq, Generic, Show)

Todo
  (LensFor todoId)
  (LensFor todoDescription)
  (LensFor todoPriority)
  (LensFor todoCreationTime)
  (LensFor todoLastUpdateTime) = tableLenses

type Todo = TodoT Identity

instance Table TodoT where
  data PrimaryKey TodoT f = TodoId (Columnar f Int32) deriving (Beamable, Generic)
  primaryKey = TodoId . _todoDueTimeId

data TodoDueTimeT f
  = TodoDueTime
  { _todoDueTimeId :: Columnar f Int32
  , _todoDueTime :: Columnar f DueTime
  }
  deriving (Beamable, Eq, Generic, Show)

type TodoDueTime = TodoDueTimeT Identity

TodoDueTime (LensFor todoDueTimeId) (LensFor todoDueTime) = tableLenses

instance Table TodoT where
  data PrimaryKey TodoT f = TodoDueTimeId (Columnar f Int32) deriving (Beamable, Generic)
  primaryKey = TodoDueTimeId . _todoId

data TodoCompletionTimeT f
  = TodoCompletionTime
  { _todoCompletionTimeId :: Columnar f Int32
  , _todoCompletionTime :: Columnar f CompletionTime
  }
  deriving (Beamable, Eq, Generic, Show)

type TodoCompletionTime = TodoCompletionTimeT Identity

TodoCompletionTime (LensFor todoCompletionTimeId) (LensFor todoCompletionTime) = tableLenses

instance Table TodoT where
  data PrimaryKey TodoT f = TodoCompletionTimeId (Columnar f Int32) deriving (Beamable, Generic)
  primaryKey = TodoCompletionTimeId . _todoCompletionTimeId

data TodoDb f
  = TodoDb
  { _todos :: f (TableEntity TodoT)
  , _todoDueTimes :: f (TableEntity TodoDueTimeT)
  , _todoCompletionTimes :: f (TableEntity TodoCompletionTimeT)
  }
  deriving (Database be, Generic)

todoDb :: DatabaseSettings be TodoDb
todoDb = defaultDbSettings

createTodo :: Connection -> Description -> Priority -> CreationTime -> Maybe DueTime -> IO Todo
createTodo connection description priority creationTime dueTime = do
  [todo] <- runBeamSqlite connection do
    runInsertReturningList
      $ insertReturning (todoDB ^. todos)
      $ insertExpressions [Todo default_ (val_ description) (val_ priority) (val_ creationTime) (val_ creationTime)]
  pure todo

createDescription :: Text -> Description
createDescription = Description

createCreationTime :: LocalTime -> CreationTime
createCreationTime = CreationTime

createDueTime :: LocalTime -> DueTime
createDueTime = DueTime

createCompletionTime :: LocalTime -> CompletionTime
createCompletionTime = CompletionTime . Just

setDescription :: Todo -> Description -> Todo
setDescription todo description = todo{description}

setDueTime :: Todo -> DueTime -> Todo
setDueTime todo dueTime = todo{dueTime}

markComplete :: Todo -> CompletionTime -> Todo
markComplete todo completionTime = todo{completionTime}

markIncomplete :: Todo -> Todo
markIncomplete todo = todo{completionTime = CompletionTime Nothing}
