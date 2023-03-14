{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Timeline.Internal
  ( -- * Core types and functions
    Timeline (..),
    peek,
    prettyTimeline,
    changes,
    TimeRange (..),
    isTimeAfterRange,

    -- * Upper bound effectiveness time handling
    Record,
    makeRecord,
    recordFrom,
    recordTo,
    recordValue,
    prettyRecord,
    fromRecords,
    Overlaps (..),
    prettyOverlaps,
    OverlapGroup (..),
    unpackOverlapGroup,
  )
where

import Data.Foldable.WithIndex (FoldableWithIndex (..))
import Data.Functor.Contravariant (Contravariant, contramap)
import Data.Functor.WithIndex (FunctorWithIndex (..))
import Data.List (intercalate, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Semigroup.Foldable.Class (fold1)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
  ( UTCTime (..),
    diffTimeToPicoseconds,
    picosecondsToDiffTime,
  )
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate, toOrdinalDate)
import Data.Traversable.WithIndex (TraversableWithIndex (..))
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import Language.Haskell.TH.Syntax qualified as TH (Lift (liftTyped))
import Language.Haskell.TH.Syntax.Compat qualified as TH
import Prelude

-- | A unbounded discrete timeline for data type @a@. @'Timeline' a@ always has
-- a value for any time, but the value can only change for a finite number of
-- times.
--
-- * 'Functor', 'Foldable' and 'Traversable' instances are provided to traverse
--   through the timeline;
-- * 'FunctorWithIndex', 'Foldable' and 'TraversableWithIndex' instances are
-- provided in case you need the current time range where each value holds
-- * 'Applicative' instance can be used to merge multiple 'Timeline's together
data Timeline t a = Timeline
  { -- | the value from negative infinity time to the first time in 'values'
    initialValue :: a,
    -- | changes are keyed by their "effective from" time, for easier lookup
    values :: Map t a
  }
  deriving stock (Show, Eq, Generic, Functor, Foldable, Traversable)

instance Ord t => Applicative (Timeline t) where
  pure :: a -> Timeline t a
  pure a = Timeline {initialValue = a, values = mempty}

  (<*>) :: forall a b. Timeline t (a -> b) -> Timeline t a -> Timeline t b
  fs@Timeline {initialValue = initialFunc, values = funcs} <*> xs@Timeline {initialValue, values} =
    Timeline
      { initialValue = initialFunc initialValue,
        values = mergedValues
      }
    where
      mergedValues :: Map t b
      mergedValues =
        Map.merge
          (Map.mapMissing $ \t f -> f $ peek xs t)
          (Map.mapMissing $ \t x -> peek fs t x)
          (Map.zipWithMatched (const ($)))
          funcs
          values

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Pretty-print @'Timeline' a@. It's provided so that you can investigate the
-- value of 'Timeline' more easily. If you need to show a timeline to the end
-- user, write your own function. We don't gurantee the result to be stable
-- across different versions of this library.
prettyTimeline :: forall t a. (Ord t, Show t, Show a) => Timeline t a -> Text
prettyTimeline Timeline {initialValue, values} =
  T.unlines $
    "\n----------Timeline--Start-------------"
      : ("initial value:                 " <> tshow initialValue)
      : fmap showOneChange (Map.toAscList values)
      ++ ["----------Timeline--End---------------"]
  where
    showOneChange :: (t, a) -> Text
    showOneChange (t, x) = "since " <> tshow t <> ": " <> tshow x

-- | Extract a single value from the timeline
peek ::
  Ord t =>
  Timeline t a ->
  -- | the time to peek
  t ->
  a
peek Timeline {..} time = maybe initialValue snd $ Map.lookupLE time values

-- | A time range. Each bound is optional. 'Nothing' represents infinity.
data TimeRange t = TimeRange
  { -- | inclusive
    from :: Maybe t,
    -- | exclusive
    to :: Maybe t
  }
  deriving stock (Show, Eq, Ord, Generic)

-- | If all time in 'TimeRange' is less than the given time
isTimeAfterRange :: Ord t => t -> TimeRange t -> Bool
isTimeAfterRange t TimeRange {to} = maybe False (t >=) to

instance Ord t => FunctorWithIndex (TimeRange t) (Timeline t) where
  imap :: (TimeRange t -> a -> b) -> Timeline t a -> Timeline t b
  imap f Timeline {..} =
    Timeline
      { initialValue = f initialRange initialValue,
        values = flip Map.mapWithKey values $ \from value ->
          let timeRange = TimeRange (Just from) (fst <$> Map.lookupGT from values)
           in f timeRange value
      }
    where
      initialRange = TimeRange Nothing $ fst <$> Map.lookupMin values

instance Ord t => FoldableWithIndex (TimeRange t) (Timeline t)

instance Ord t => TraversableWithIndex (TimeRange t) (Timeline t) where
  itraverse :: (Applicative f) => (TimeRange t -> a -> f b) -> Timeline t a -> f (Timeline t b)
  itraverse f = sequenceA . imap f

-- | Return the set of time when the value changes
changes :: Timeline t a -> Set t
changes Timeline {values} = Map.keysSet values

-- | A value with @effectiveFrom@ and @effectiveTo@ attached. This is often the
-- type we get from inputs. A list of @'Record' a@ can be converted to
-- @'Timeline' ('Maybe' a)@. See 'fromRecords'.
data Record t a = Record
  { -- | inclusive
    from :: t,
    -- | exclusive. When 'Nothing', the record never expires, until there is
    -- another record with a newer 'effectiveFrom' time.
    to :: Maybe t,
    value :: a
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable, TH.Lift)

-- | Get the "effective from" time
recordFrom :: Record t a -> t
recordFrom Record {from} = from

-- | Get the "effective to" time
recordTo :: Record t a -> Maybe t
recordTo Record {to} = to

-- | Get the value wrapped in a @'Record' a@
recordValue :: Record t a -> a
recordValue = value

-- | A smart constructor for @'Record' a@.
-- Returns 'Nothing' if @effectiveTo@ is not greater than @effectiveFrom@
makeRecord ::
  Ord t =>
  -- | effective from
  t ->
  -- | optional effective to
  Maybe t ->
  -- | value
  a ->
  Maybe (Record t a)
makeRecord from to value =
  if maybe False (from >=) to
    then Nothing
    else Just Record {..}

-- | Special support for 'UTCTime'. This will be removed when 'TH.Lift'
-- instances are provided by the @time@ package directly.
instance {-# OVERLAPPING #-} (TH.Lift a) => TH.Lift (Record UTCTime a) where
  liftTyped Record {..} =
    [||
    Record
      (unLiftUTCTime $$(TH.liftTyped $ LiftUTCTime from))
      (fmap unLiftUTCTime $$(TH.liftTyped $ LiftUTCTime <$> to))
      $$(TH.liftTyped value)
    ||]

newtype LiftUTCTime = LiftUTCTime UTCTime
  deriving stock (Generic)

unLiftUTCTime :: LiftUTCTime -> UTCTime
unLiftUTCTime (LiftUTCTime t) = t

instance TH.Lift LiftUTCTime where
  liftTyped (LiftUTCTime (UTCTime (toOrdinalDate -> (year, day)) diffTime)) =
    [||
    LiftUTCTime $
      UTCTime
        (fromOrdinalDate $$(TH.liftTyped year) $$(TH.liftTyped day))
        (picosecondsToDiffTime $$(TH.liftTyped (diffTimeToPicoseconds diffTime)))
    ||]

-- | Pretty-print @'Record' a@, like 'prettyTimeline'.
prettyRecord :: (Show t, Show a) => Record t a -> Text
prettyRecord Record {..} = tshow from <> " ~ " <> tshow to <> ": " <> tshow value

-- | An @'Overlaps' a@ consists of several groups. Within each group, all
-- records are connected. Definition of connectivity: two records are
-- "connected" if and only if they overlap.
newtype Overlaps t a = Overlaps {groups :: NonEmpty (OverlapGroup t a)}
  deriving newtype (Semigroup)
  deriving stock (Show, Eq, Generic)

-- | Pretty-print @'Overlaps' a@, like 'prettyTimeline'.
prettyOverlaps :: (Show t, Show a) => Overlaps t a -> Text
prettyOverlaps Overlaps {groups} =
  "Here are "
    <> tshow (length groups)
    <> " group(s) of overlapping records\n"
    <> sep
    <> T.intercalate sep (prettyOverlapGroup <$> NonEmpty.toList groups)
    <> sep
  where
    sep = "--------------------\n"

-- | A group of overlapping records. There must be at least two records within a group.
data OverlapGroup t a = OverlapGroup (Record t a) (Record t a) [Record t a]
  deriving stock (Show, Eq, Generic)

prettyOverlapGroup :: (Show t, Show a) => OverlapGroup t a -> Text
prettyOverlapGroup = T.unlines . fmap prettyRecord . unpackOverlapGroup

-- | Unpack @'OverlapGroup' a@ as a list of records.
unpackOverlapGroup :: OverlapGroup t a -> [Record t a]
unpackOverlapGroup (OverlapGroup r1 r2 records) = r1 : r2 : records

-- | Build a 'Timeline' from a list of 'Record's.
--
-- For any time, there could be zero, one, or more values, according to the
-- input. No other condition is possible. We have taken account the "zero" case
-- by wrapping the result in 'Maybe', so the only possible error is 'Overlaps'.
-- The 'Traversable' instance of @'Timeline' a@ can be used to convert
-- @'Timeline' ('Maybe' a)@ to @'Maybe' ('Timeline' a)@
fromRecords :: forall t a. Ord t => [Record t a] -> Either (Overlaps t a) (Timeline t (Maybe a))
fromRecords records =
  maybe (Right timeline) Left overlaps
  where
    sortedRecords = sortOn recordFrom records

    -- overlap detection
    overlaps =
      fmap fold1
        . nonEmpty
        . mapMaybe checkForOverlap
        . foldr mergeOverlappingNeighbours []
        $ sortedRecords

    mergeOverlappingNeighbours ::
      Record t a ->
      [NonEmpty (Record t a)] ->
      [NonEmpty (Record t a)]
    mergeOverlappingNeighbours current ((next :| group) : groups)
      -- Be aware that this is called in 'foldr', so it traverse the list from
      -- right to left. If the current record overlaps with the top (left-most)
      -- record in the next group, we add it to the group. Otherwise, create a
      -- new group for it.
      | isOverlapping = (current NonEmpty.<| next :| group) : groups
      | otherwise = (current :| []) : (next :| group) : groups
      where
        isOverlapping = maybe False (recordFrom next <) (recordTo current)
    mergeOverlappingNeighbours current [] = [current :| []]

    checkForOverlap :: NonEmpty (Record t a) -> Maybe (Overlaps t a)
    checkForOverlap (_ :| []) = Nothing
    checkForOverlap (x1 :| x2 : xs) = Just . Overlaps . (:| []) $ OverlapGroup x1 x2 xs

    -- build the timeline assuming all elements of `sortedRecords` cover
    -- distinct (non-overlapping) time-periods
    timeline :: Timeline t (Maybe a)
    timeline =
      case nonEmpty sortedRecords of
        Nothing -> pure Nothing
        Just records' ->
          Timeline
            { initialValue = Nothing,
              values =
                Map.fromList . concat $
                  zipWith
                    connectAdjacentRecords
                    (NonEmpty.toList records')
                    ((Just <$> NonEmpty.tail records') <> [Nothing])
            }
    connectAdjacentRecords :: Record t a -> Maybe (Record t a) -> [(t, Maybe a)]
    connectAdjacentRecords current next =
      (recordFrom current, Just $ value current)
        : maybeToList gap
      where
        gap = do
          effectiveTo' <- recordTo current
          if maybe True (\next' -> effectiveTo' < recordFrom next') next
            then pure (effectiveTo', Nothing)
            else Nothing
