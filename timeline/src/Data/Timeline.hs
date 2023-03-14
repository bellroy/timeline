{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
--  Data type representing a piecewise-constant function over time.
module Data.Timeline
  ( module Data.Timeline.Internal,
    makeRecordTH,
  )
where

import Data.Time (UTCTime)
import Data.Timeline.Internal
import Language.Haskell.TH.Syntax qualified as TH
import Language.Haskell.TH.Syntax.Compat qualified as TH

-- | Template Haskell counterpart of 'makeRecord'.
makeRecordTH ::
  (Ord t, TH.Lift (Record t a)) =>
  t ->
  Maybe t ->
  a ->
  TH.SpliceQ (Record t a)
makeRecordTH effectiveFrom effectiveTo value =
  TH.bindSplice
    ( maybe (fail "effective to is no greater than effective from") pure $
        makeRecord effectiveFrom effectiveTo value
    )
    TH.liftTyped
