{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Timeline
  ( module Data.Timeline.Internal,
    makeRecordTH,
  )
where

import Data.Time (UTCTime)
import Data.Timeline.Internal
import Language.Haskell.TH (Quote)
import Language.Haskell.TH.Syntax qualified as TH

makeRecordTH ::
  (MonadFail m, Quote m, TH.Lift a) => UTCTime -> Maybe UTCTime -> a -> TH.Code m (Record a)
makeRecordTH effectiveFrom effectiveTo value = TH.bindCode
  ( maybe (fail "effective to is no greater than effective from") pure $
      makeRecord effectiveFrom effectiveTo value
  )
  $ \record -> [||$$(TH.liftTyped record)||]
