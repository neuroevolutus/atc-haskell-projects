{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TodoManager.Types.Getters
  ( completionTimeToZonedTime
  , completionTimeToUtc
  , creationTimeToZonedTime
  , creationTimeToUtc
  , dueTimeToZonedTime
  , dueTimeToUtc
  , lastUpdateTimeToZonedTime
  , lastUpdateTimeToUtc
  ) where

import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)

import TodoManager.Types.Internal
  ( CompletionTime (CompletionTime)
  , CreationTime (CreationTime)
  , DueTime (DueTime)
  , LastUpdateTime (LastUpdateTime)
  )

completionTimeToZonedTime :: CompletionTime -> ZonedTime
completionTimeToZonedTime (CompletionTime time) = time

completionTimeToUtc :: CompletionTime -> UTCTime
completionTimeToUtc (CompletionTime time) = zonedTimeToUTC time

creationTimeToZonedTime :: CreationTime -> ZonedTime
creationTimeToZonedTime (CreationTime time) = time

creationTimeToUtc :: CreationTime -> UTCTime
creationTimeToUtc (CreationTime time) = zonedTimeToUTC time

dueTimeToZonedTime :: DueTime -> ZonedTime
dueTimeToZonedTime (DueTime time) = time

dueTimeToUtc :: DueTime -> UTCTime
dueTimeToUtc (DueTime time) = zonedTimeToUTC time

lastUpdateTimeToZonedTime :: LastUpdateTime -> ZonedTime
lastUpdateTimeToZonedTime (LastUpdateTime time) = time

lastUpdateTimeToUtc :: LastUpdateTime -> UTCTime
lastUpdateTimeToUtc (LastUpdateTime time) = zonedTimeToUTC time
