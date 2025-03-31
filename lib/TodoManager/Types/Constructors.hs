{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TodoManager.Types.Constructors (makeCompletionTime, makeCreationTime, makeDescription, makeDueTime, makeLastUpdateTime) where

import Data.Text (Text)
import Data.Time.LocalTime (ZonedTime)

import TodoManager.Types.Internal
  ( CompletionTime (CompletionTime)
  , CreationTime (CreationTime)
  , Description (Description)
  , DueTime (DueTime)
  , LastUpdateTime (LastUpdateTime)
  )

makeDescription :: Text -> Description
makeDescription = Description

makeCreationTime :: ZonedTime -> CreationTime
makeCreationTime = CreationTime

makeLastUpdateTime :: ZonedTime -> LastUpdateTime
makeLastUpdateTime = LastUpdateTime

makeDueTime :: ZonedTime -> DueTime
makeDueTime = DueTime

makeCompletionTime :: ZonedTime -> CompletionTime
makeCompletionTime = CompletionTime
