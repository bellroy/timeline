{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
--  Hedgehog generators for the timeline library.
module Data.Timeline.Hedgehog
  ( -- * Timeline Generators
    gen,
    genRecord,

    -- * Helpers
    genUTCTime,
  )
where

import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Timeline
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | Generator for @'Timeline' a@
gen ::
  (MonadGen m) =>
  -- | Generator for values
  m a ->
  m (Timeline a)
gen genValue = do
  tlInitialValue <- genValue
  tlValues <- Gen.map (Range.linear 0 20) $ (,) <$> genUTCTime <*> genValue
  pure Timeline {tlInitialValue, tlValues}

-- | Generator for @'Record' a@
genRecord ::
  (MonadGen m) =>
  -- | Generator for the value
  m a ->
  m (Record a)
genRecord valueGen =
  Gen.justT $ do
    t1 <- genUTCTime
    t2 <- Gen.maybe $ Gen.filterT (/= t1) genUTCTime
    makeRecord t1 t2 <$> valueGen

-- | A 'UTCTime' generator
genUTCTime :: (MonadGen m) => m UTCTime
genUTCTime = do
  y <- toInteger <$> Gen.int (Range.constant 2000 2030)
  m <- Gen.int (Range.constant 1 12)
  d <- Gen.int (Range.constant 1 28)
  let day = fromGregorian y m d
  secs <- toInteger <$> Gen.int (Range.constant 0 86401)
  let diff = secondsToDiffTime secs
  pure $ UTCTime day diff
