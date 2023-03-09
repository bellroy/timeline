{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Timeline
  ( module Data.Timeline.Internal,
    makeRecordTH,
  )
where

import Data.Time (UTCTime)
import Data.Timeline.Internal
import Language.Haskell.TH.Syntax qualified as TH
import Language.Haskell.TH.Syntax.Compat qualified as TH

makeRecordTH ::
  (TH.Lift a) => UTCTime -> Maybe UTCTime -> a -> TH.SpliceQ (Record a)
makeRecordTH effectiveFrom effectiveTo value =
  TH.bindSplice
    ( maybe (fail "effective to is no greater than effective from") pure $
        makeRecord effectiveFrom effectiveTo value
    )
    TH.liftTyped
