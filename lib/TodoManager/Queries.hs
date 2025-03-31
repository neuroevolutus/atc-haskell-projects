{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | SQL queries
module TodoManager.Queries
  ( deleteTodo
  , insertTodo
  , selectAllTodos
  , setCompletionTime
  , setDescription
  , setDueTime
  , unsetCompletionTime
  , unsetDueTime
  ) where

import Control.Applicative (pure)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Function (const, ($), (.))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe, listToMaybe, maybe)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Query
  ( all_
  , default_
  , delete
  , guard_
  , insert
  , insertExpressions
  , leftJoin_
  , references_
  , runDelete
  , runInsert
  , runSelectReturningList
  , runUpdate
  , select
  , update
  , val_
  , (<-.)
  , (==.)
  )
import Database.Beam.Sqlite.Connection (SqliteM)
import Lens.Micro ((^.))

import TodoManager.Lenses
  ( todoCompletionTime
  , todoCompletionTimeTodoId
  , todoCompletionTimes
  , todoDescription
  , todoDueTime
  , todoDueTimeTodoId
  , todoDueTimes
  , todoId
  , todos
  )
import TodoManager.Types.Internal
  ( CompletionTime
  , CreationTime (CreationTime)
  , Description
  , DueTime
  , LastUpdateTime (LastUpdateTime)
  , PrimaryKey (TodoId)
  , Priority
  , Todo
  , TodoCompletionTimeT (TodoCompletionTimeT, _todoCompletionTime, _todoCompletionTimeTodoId)
  , TodoDueTimeT (TodoDueTimeT, _todoDueTime, _todoDueTimeTodoId)
  , TodoT (TodoT, _todoCreationTime, _todoDescription, _todoId, _todoLastUpdateTime, _todoPriority)
  , todoDb
  )

insertDueTime :: DueTime -> PrimaryKey TodoT Identity -> SqliteM ()
insertDueTime dueTime todoParentId =
  runInsert . insert (todoDb ^. todoDueTimes)
    $ insertExpressions [TodoDueTimeT default_ (val_ dueTime) (val_ todoParentId)]

insertCompletionTime :: CompletionTime -> PrimaryKey TodoT Identity -> SqliteM ()
insertCompletionTime completionTime todoParentId =
  runInsert . insert (todoDb ^. todoCompletionTimes)
    $ insertExpressions [TodoCompletionTimeT default_ (val_ completionTime) (val_ todoParentId)]

insertTodo :: Description -> Priority -> CreationTime -> Maybe DueTime -> SqliteM ()
insertTodo description priority creationTime@(CreationTime unwrappedCreationTime) maybeDueTime = do
  [todo] <-
    runInsertReturningList
      $ insert (todoDb ^. todos)
      $ insertExpressions
        [TodoT default_ (val_ description) (val_ priority) (val_ creationTime) (val_ (LastUpdateTime unwrappedCreationTime))]
  let todoParentId = TodoId $ todo ^. todoId
  void $ runMaybeT do
    dueTime <- MaybeT $ pure maybeDueTime
    lift $ insertDueTime dueTime todoParentId

setDescription :: Description -> Description -> SqliteM ()
setDescription oldDescription newDescription = do
  runUpdate
    $ update
      (todoDb ^. todos)
      (\description_ -> description_ ^. todoDescription <-. val_ newDescription)
      (\description_ -> description_ ^. todoDescription ==. val_ oldDescription)

deleteTodo :: Description -> SqliteM ()
deleteTodo description =
  runDelete $ delete (todoDb ^. todos) (\todo_ -> todo_ ^. todoDescription ==. val_ description)

selectAllTodos :: SqliteM [(Description, Priority, CreationTime, LastUpdateTime, Maybe DueTime, Maybe CompletionTime)]
selectAllTodos = runSelectReturningList
  $ select do
    todo <- all_ (todoDb ^. todos)
    dueTime <- leftJoin_ (all_ (todoDb ^. todoDueTimes)) (\dueTime -> _todoDueTimeTodoId dueTime `references_` todo)
    completionTime <-
      leftJoin_
        (all_ (todoDb ^. todoCompletionTimes))
        (\completionTime -> _todoCompletionTimeTodoId completionTime `references_` todo)
    pure
      ( _todoDescription todo
      , _todoPriority todo
      , _todoCreationTime todo
      , _todoLastUpdateTime todo
      , _todoDueTime dueTime
      , _todoCompletionTime completionTime
      )

updateDueTime
  :: PrimaryKey TodoT Identity -> DueTime -> SqliteM ()
updateDueTime (TodoId todoId_) dueTime =
  runUpdate
    $ update
      (todoDb ^. todoDueTimes)
      (\dueTime_ -> dueTime_ ^. todoDueTime <-. val_ dueTime)
      (\dueTime_ -> dueTime_ ^. todoDueTimeTodoId ==. val_ todoId_)

updateCompletionTime
  :: PrimaryKey TodoT Identity -> CompletionTime -> SqliteM ()
updateCompletionTime (TodoId todoId_) completionTime =
  runUpdate
    $ update
      (todoDb ^. todoCompletionTimes)
      (\completionTime_ -> completionTime_ ^. todoCompletionTime <-. val_ completionTime)
      (\completionTime_ -> completionTime_ ^. todoCompletionTimeTodoId ==. val_ todoId_)

deleteDueTime
  :: PrimaryKey TodoT Identity -> SqliteM ()
deleteDueTime (TodoId todoId_) =
  runDelete
    $ delete
      (todoDb ^. todoDueTimes)
      (\dueTime_ -> dueTime_ ^. todoDueTimeTodoId ==. val_ todoId_)

deleteCompletionTime
  :: PrimaryKey TodoT Identity -> SqliteM ()
deleteCompletionTime (TodoId todoId_) =
  runDelete
    $ delete
      (todoDb ^. todoCompletionTimes)
      (\completionTime_ -> completionTime_ ^. todoCompletionTimeTodoId ==. val_ todoId_)

selectTodoWithDescription :: Description -> SqliteM Todo
selectTodoWithDescription description = do
  [todo] <- runSelectReturningList $ select do
    todo <- all_ (todoDb ^. todos)
    guard_ (todo ^. todoDescription ==. val_ description)
    pure todo
  pure todo

setDueTime :: Description -> DueTime -> SqliteM ()
setDueTime description dueTime = do
  todo <- selectTodoWithDescription description
  let todoParentId = TodoId $ todo ^. todoId
  dueTimeTodoIdList <- runSelectReturningList $ select do
    dueTime_ <- all_ (todoDb ^. todoDueTimes)
    guard_ (dueTime_ ^. todoDueTimeTodoId ==. val_ (todo ^. todoId))
    pure (_todoDueTimeTodoId dueTime_)
  maybe (insertDueTime dueTime todoParentId) (const $ updateDueTime todoParentId dueTime) $ listToMaybe dueTimeTodoIdList

setCompletionTime :: Description -> CompletionTime -> SqliteM ()
setCompletionTime description completionTime = do
  todo <- selectTodoWithDescription description
  let todoParentId = TodoId $ todo ^. todoId
  completionTimeTodoIdList <- runSelectReturningList $ select do
    completionTime_ <- all_ (todoDb ^. todoCompletionTimes)
    guard_ (completionTime_ ^. todoCompletionTimeTodoId ==. val_ (todo ^. todoId))
    pure (_todoCompletionTimeTodoId completionTime_)
  maybe (insertCompletionTime completionTime todoParentId) (const $ updateCompletionTime todoParentId completionTime)
    $ listToMaybe completionTimeTodoIdList

unsetDueTime :: Description -> SqliteM ()
unsetDueTime description = do
  todo <- selectTodoWithDescription description
  let todoParentId = TodoId $ todo ^. todoId
  deleteDueTime todoParentId

unsetCompletionTime :: Description -> SqliteM ()
unsetCompletionTime description = do
  todo <- selectTodoWithDescription description
  let todoParentId = TodoId $ todo ^. todoId
  deleteCompletionTime todoParentId
