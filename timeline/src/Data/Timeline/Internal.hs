{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  ( -- * Common
    Timeline (..),
    fromValues,
    peek,
    prettyTimeline,
    changes,
    TimeRange (..),
    isTimeAfterRange,

    -- * effectiveFrom + optional effectiveTo
    Record,
    makeRecord,
    getEffectiveFrom,
    getEffectiveTo,
    getValue,
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

-- | An infinite, discrete timeline for data type @a@.
-- It always has a value for any time, but the set of values has a finite size.
data Timeline a = Timeline
  {- Internally this is represented by an initial value and a list of updated values.
    Each change has an effective time and a new value.
    This makes it possible to inspect the individual changes and to serialize the type,
      but makes it impossible to represent timelines that change continuously. Continuous data
      in computers' world is very rare, unless you are working with some math formulas.
  -}
  { initialValue :: a,
    -- | changes are keyed by their "effective from" time, for easier lookup
    values :: Map UTCTime a
  }
  deriving stock (Show, Eq, Generic, Functor, Foldable, Traversable)

fromValues ::
  -- | initial value
  a ->
  -- | new values begin to take effect at the specified times
  Map UTCTime a ->
  Timeline a
fromValues initialValue values = Timeline {initialValue, values}

instance Applicative Timeline where
  pure :: a -> Timeline a
  pure a = Timeline {initialValue = a, values = mempty}

  (<*>) :: forall a b. Timeline (a -> b) -> Timeline a -> Timeline b
  fs@Timeline {initialValue = initialFunc, values = funcs} <*> xs@Timeline {initialValue, values} =
    Timeline
      { initialValue = initialFunc initialValue,
        values = mergedValues
      }
    where
      mergedValues :: Map UTCTime b
      mergedValues =
        Map.merge
          (Map.mapMissing $ \t f -> f $ peek xs t)
          (Map.mapMissing $ \t x -> peek fs t x)
          (Map.zipWithMatched (const ($)))
          funcs
          values

tshow :: Show a => a -> Text
tshow = T.pack . show

prettyTimeline :: forall a. Show a => Timeline a -> Text
prettyTimeline Timeline {initialValue, values} =
  T.unlines $
    "\n----------Timeline--Start-------------"
      : ("initial value:                 " <> tshow initialValue)
      : fmap showOneChange (Map.toAscList values)
      ++ ["----------Timeline--End---------------"]
  where
    showOneChange :: (UTCTime, a) -> Text
    showOneChange (t, x) = "since " <> tshow t <> ": " <> tshow x

peek :: Timeline a -> UTCTime -> a
peek Timeline {..} time = maybe initialValue snd $ Map.lookupLE time values

data TimeRange = TimeRange
  { from :: Maybe UTCTime,
    to :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic)

isTimeAfterRange :: UTCTime -> TimeRange -> Bool
isTimeAfterRange t TimeRange {to} = maybe False (t >=) to

instance FunctorWithIndex TimeRange Timeline where
  imap :: (TimeRange -> a -> b) -> Timeline a -> Timeline b
  imap f Timeline {..} =
    Timeline
      { initialValue = f initialRange initialValue,
        values = flip Map.mapWithKey values $ \from value ->
          let timeRange = TimeRange (Just from) (fst <$> Map.lookupGT from values)
           in f timeRange value
      }
    where
      initialRange = TimeRange Nothing $ fst <$> Map.lookupMin values

instance FoldableWithIndex TimeRange Timeline

instance TraversableWithIndex TimeRange Timeline where
  itraverse :: (Applicative f) => (TimeRange -> a -> f b) -> Timeline a -> f (Timeline b)
  itraverse f = sequenceA . imap f

changes :: Timeline a -> Set UTCTime
changes Timeline {values} = Map.keysSet values

data Record a = Record
  { effectiveFrom :: UTCTime,
    effectiveTo :: Maybe UTCTime,
    value :: a
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

getEffectiveFrom :: Record a -> UTCTime
getEffectiveFrom = effectiveFrom

getEffectiveTo :: Record a -> Maybe UTCTime
getEffectiveTo = effectiveTo

getValue :: Record a -> a
getValue = value

-- | 'makeRecord' returns 'Nothing' if @effectiveTo@ is not greater than @effectiveFrom@
makeRecord ::
  -- | effective from
  UTCTime ->
  -- | optional effective to
  Maybe UTCTime ->
  -- | value
  a ->
  Maybe (Record a)
-- can't write a makeRecordTH because UTCTime has no Lift instance
makeRecord effectiveFrom effectiveTo value =
  if maybe False (effectiveFrom >=) effectiveTo
    then Nothing
    else Just Record {..}

instance (TH.Lift a) => TH.Lift (Record a) where
  liftTyped Record {..} =
    [||
    Record
      (unLiftUTCTime $$(TH.liftTyped $ LiftUTCTime effectiveFrom))
      (fmap unLiftUTCTime $$(TH.liftTyped $ LiftUTCTime <$> effectiveTo))
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

prettyRecord :: Show a => Record a -> Text
prettyRecord Record {..} = tshow effectiveFrom <> " ~ " <> tshow effectiveTo <> ": " <> tshow value

newtype Overlaps a = Overlaps {groups :: NonEmpty (OverlapGroup a)}
  deriving newtype (Semigroup)
  deriving stock (Show, Eq, Generic)

prettyOverlaps :: Show a => Overlaps a -> Text
prettyOverlaps Overlaps {groups} =
  "Here are "
    <> tshow (length groups)
    <> " group(s) of overlapping records\n"
    <> sep
    <> T.intercalate sep (prettyOverlapGroup <$> NonEmpty.toList groups)
    <> sep
  where
    sep = "--------------------\n"

data OverlapGroup a = OverlapGroup (Record a) (Record a) [Record a]
  deriving stock (Show, Eq, Generic)

prettyOverlapGroup :: Show a => OverlapGroup a -> Text
prettyOverlapGroup = T.unlines . fmap prettyRecord . unpackOverlapGroup

unpackOverlapGroup :: OverlapGroup a -> [Record a]
unpackOverlapGroup (OverlapGroup r1 r2 records) = r1 : r2 : records

-- | Build a 'Timeline' from a list of 'Record's.
--
-- For any time, there could be zero, one, or more values, according to the input. No other condition
-- is possible. We have taken account the "zero" case by wrapping the result in 'Maybe', so the only
-- possible error is 'Overlaps'.
fromRecords :: forall a. [Record a] -> Either (Overlaps a) (Timeline (Maybe a))
fromRecords records =
  maybe (Right timeline) Left overlaps
  where
    sortedRecords = sortOn effectiveFrom records

    -- overlap detection
    overlaps =
      fmap fold1
        . nonEmpty
        . mapMaybe checkForOverlap
        . foldr mergeOverlappingNeighbours []
        $ sortedRecords

    mergeOverlappingNeighbours ::
      Record a ->
      [NonEmpty (Record a)] ->
      [NonEmpty (Record a)]
    mergeOverlappingNeighbours current ((next :| group) : groups)
      -- Be aware that this is called in 'foldr', so it traverse the list from right to left.
      -- If the current record overlaps with the top (left-most) record in the next group, we add it
      -- to the group. Otherwise, create a new group for it.
      | isOverlapping = (current NonEmpty.<| next :| group) : groups
      | otherwise = (current :| []) : (next :| group) : groups
      where
        isOverlapping = maybe False (effectiveFrom next <) (effectiveTo current)
    mergeOverlappingNeighbours current [] = [current :| []]

    checkForOverlap :: NonEmpty (Record a) -> Maybe (Overlaps a)
    checkForOverlap (_ :| []) = Nothing
    checkForOverlap (x1 :| x2 : xs) = Just . Overlaps . (:| []) $ OverlapGroup x1 x2 xs

    -- build the timeline assuming all elements of `sortedRecords` cover distinct (non-overlapping) time-periods
    timeline :: Timeline (Maybe a)
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
    connectAdjacentRecords :: Record a -> Maybe (Record a) -> [(UTCTime, Maybe a)]
    connectAdjacentRecords current next =
      (effectiveFrom current, Just $ value current)
        : maybeToList gap
      where
        gap = do
          effectiveTo' <- effectiveTo current
          if maybe True (\next' -> effectiveTo' < effectiveFrom next') next
            then pure (effectiveTo', Nothing)
            else Nothing
