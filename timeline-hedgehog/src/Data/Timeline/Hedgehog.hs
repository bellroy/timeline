{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
--  Hedgehog generators for the timeline library.
module Data.Timeline.Hedgehog
  ( -- * Timeline Generators
    gen,
    genRecord,
  )
where

import Data.Timeline
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | Generator for @'Timeline' a@
gen ::
  (MonadGen m, Ord t) =>
  m t ->
  -- | Generator for values
  m a ->
  m (Timeline t a)
gen genTime genValue = do
  initialValue <- genValue
  values <- Gen.map (Range.linear 0 20) $ (,) <$> genTime <*> genValue
  pure Timeline {initialValue, values}

-- | Generator for @'Record' a@
genRecord ::
  (MonadGen m, Ord t) =>
  m t ->
  -- | Generator for the value
  m a ->
  m (Record t a)
genRecord genTime genValue =
  Gen.justT $ do
    t1 <- genTime
    t2 <- Gen.maybe $ Gen.filterT (/= t1) genTime
    makeRecord t1 t2 <$> genValue
