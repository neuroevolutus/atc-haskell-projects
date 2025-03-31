{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module TodoManager.Lenses
  ( todoId
  , todoDescription
  , todoPriority
  , todoCreationTime
  , todoLastUpdateTime
  , todoDueTimeId
  , todoDueTime
  , todoDueTimeTodoId
  , todoCompletionTimeId
  , todoCompletionTime
  , todoCompletionTimeTodoId
  , todoDueTimes
  , todoCompletionTimes
  , todos
  )
where

import Data.Int (Int32)
import Database.Beam (Columnar)
import Database.Beam.Schema (LensFor (LensFor), TableEntity, TableLens (TableLens), dbLenses, tableLenses)
import Lens.Micro (Lens)

import TodoManager.Types.Internal
  ( CompletionTime
  , CreationTime
  , Description
  , DueTime
  , LastUpdateTime
  , PrimaryKey (TodoId)
  , Priority
  , TodoCompletionTimeT (TodoCompletionTimeT)
  , TodoDb (TodoDb)
  , TodoDueTimeT (TodoDueTimeT)
  , TodoT (TodoT)
  )

todoId :: forall f. Lens (TodoT f) (TodoT f) (Columnar f Int32) (Columnar f Int32)
TodoT (LensFor todoId) _ _ _ _ = tableLenses

todoDescription :: forall f. Lens (TodoT f) (TodoT f) (Columnar f Description) (Columnar f Description)
TodoT _ (LensFor todoDescription) _ _ _ = tableLenses

todoPriority :: forall f. Lens (TodoT f) (TodoT f) (Columnar f Priority) (Columnar f Priority)
TodoT _ _ (LensFor todoPriority) _ _ = tableLenses

todoCreationTime :: forall f. Lens (TodoT f) (TodoT f) (Columnar f CreationTime) (Columnar f CreationTime)
TodoT _ _ _ (LensFor todoCreationTime) _ = tableLenses

todoLastUpdateTime :: forall f. Lens (TodoT f) (TodoT f) (Columnar f LastUpdateTime) (Columnar f LastUpdateTime)
TodoT _ _ _ _ (LensFor todoLastUpdateTime) = tableLenses

todoDueTimeId :: forall f. Lens (TodoDueTimeT f) (TodoDueTimeT f) (Columnar f Int32) (Columnar f Int32)
TodoDueTimeT (LensFor todoDueTimeId) _ _ = tableLenses

todoDueTime :: forall f. Lens (TodoDueTimeT f) (TodoDueTimeT f) (Columnar f DueTime) (Columnar f DueTime)
TodoDueTimeT _ (LensFor todoDueTime) _ = tableLenses

todoDueTimeTodoId :: forall f. Lens (TodoDueTimeT f) (TodoDueTimeT f) (Columnar f Int32) (Columnar f Int32)
TodoDueTimeT _ _ (TodoId (LensFor todoDueTimeTodoId)) = tableLenses

todoCompletionTimeId
  :: forall f. Lens (TodoCompletionTimeT f) (TodoCompletionTimeT f) (Columnar f Int32) (Columnar f Int32)
TodoCompletionTimeT (LensFor todoCompletionTimeId) _ _ = tableLenses

todoCompletionTime
  :: forall f. Lens (TodoCompletionTimeT f) (TodoCompletionTimeT f) (Columnar f CompletionTime) (Columnar f CompletionTime)
TodoCompletionTimeT _ (LensFor todoCompletionTime) _ = tableLenses

todoCompletionTimeTodoId
  :: forall f. Lens (TodoCompletionTimeT f) (TodoCompletionTimeT f) (Columnar f Int32) (Columnar f Int32)
TodoCompletionTimeT _ _ (TodoId (LensFor todoCompletionTimeTodoId)) = tableLenses

todos :: forall f. Lens (TodoDb f) (TodoDb f) (f (TableEntity TodoT)) (f (TableEntity TodoT))
TodoDb (TableLens todos) _ _ = dbLenses

todoDueTimes :: forall f. Lens (TodoDb f) (TodoDb f) (f (TableEntity TodoDueTimeT)) (f (TableEntity TodoDueTimeT))
TodoDb _ (TableLens todoDueTimes) _ = dbLenses

todoCompletionTimes
  :: forall f. Lens (TodoDb f) (TodoDb f) (f (TableEntity TodoCompletionTimeT)) (f (TableEntity TodoCompletionTimeT))
TodoDb _ _ (TableLens todoCompletionTimes) = dbLenses
