{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Data.TimelineTest where

import Control.Applicative (liftA2)
import Control.Monad.Trans.Writer.CPS (execWriter, tell)
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.WithIndex (imap)
import Data.Hashable
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
  ( UTCTime (UTCTime),
    addUTCTime,
    fromGregorian,
    secondsToDiffTime,
    secondsToNominalDiffTime,
  )
import Data.Timeline
import Data.Timeline.Hedgehog (gen)
import Hedgehog (MonadGen, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@=?), (@?=))
import Test.Tasty.Hedgehog (testProperty)

test_makeRecord :: [TestTree]
test_makeRecord =
  [ testProperty "it's always valid to have no effective-to" $ property $ do
      t <- forAll genUTCTime
      isJust (makeRecord @UTCTime @Int t Nothing 1) === True,
    testProperty "effectiveFrom must be less than effective-to" $ property $ do
      t1 <- forAll genUTCTime
      t2 <- forAll genUTCTime
      let tMin = min t1 t2
          tMax = max t1 t2
      if t1 == t2
        then isNothing (makeRecord @UTCTime @Int t1 (Just t2) 1) === True
        else isJust (makeRecord @UTCTime @Int tMin (Just tMax) 1) === True
  ]

test_fromRecords :: [TestTree]
test_fromRecords =
  [ testCase "empty input" $
      fromRecords @UTCTime @Int [] @?= Right (pure Nothing),
    testCase'
      "one change"
      [ makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 1 26) 7200)
          Nothing
          100
      ],
    testCase'
      "one change, with effective to"
      [ makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 1 26) 7200)
          (Just $ UTCTime (fromGregorian 2023 2 28) 0)
          100
      ],
    testCase'
      "all non-overlapping situations together"
      [ makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 1 26) 7200)
          (Just $ UTCTime (fromGregorian 2023 2 28) 0)
          100,
        makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 3 1) 0)
          (Just $ UTCTime (fromGregorian 2023 4 1) 0)
          200,
        makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 4 1) 0)
          Nothing
          300,
        makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 5 1) 0)
          Nothing
          400,
        makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 6 1) 0)
          Nothing
          500
      ],
    testCase'
      "overlaps"
      [ makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 1 26) 7200)
          (Just $ UTCTime (fromGregorian 2023 2 28) 0)
          100,
        makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 2 27) 0)
          (Just $ UTCTime (fromGregorian 2023 3 5) 0)
          200,
        makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 3 1) 0)
          Nothing
          300
      ],
    testCase'
      "two groups of overlap"
      [ makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 1 26) 7200)
          (Just $ UTCTime (fromGregorian 2023 2 28) 0)
          100,
        makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 3 1) 0)
          (Just $ UTCTime (fromGregorian 2023 3 5) 0)
          200,
        makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 3 3) 0)
          (Just $ UTCTime (fromGregorian 2023 3 4) 0)
          300,
        makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 3 6) 0)
          (Just $ UTCTime (fromGregorian 2023 3 8) 0)
          400,
        makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 3 8) 0)
          (Just $ UTCTime (fromGregorian 2023 3 15) 0)
          500,
        makeRecord @UTCTime @Int
          (UTCTime (fromGregorian 2023 3 14) 0)
          Nothing
          600
      ]
  ]
  where
    testCase' :: (Show a) => TestName -> [Maybe (Record UTCTime a)] -> TestTree
    testCase' name = buildGoldenTest pretty name . fromRecords . catMaybes

    pretty (Left overlaps) = prettyOverlaps overlaps
    pretty (Right timeline) = prettyTimeline timeline

test_peek :: [TestTree]
test_peek =
  [ testCase "constant" $ 1 @=? peek @UTCTime @Int (pure 1) (UTCTime (fromGregorian 2023 1 26) 0),
    testCase "before first change" $
      1
        @=? peek @UTCTime @Int
          (Timeline 1 (Map.singleton (UTCTime (fromGregorian 2023 1 16) 0) 2))
          (UTCTime (fromGregorian 2023 1 15) 0),
    testCase "between changes" $
      2
        @=? peek @UTCTime @Int
          ( Timeline
              1
              [ (UTCTime (fromGregorian 2023 1 16) 0, 2),
                (UTCTime (fromGregorian 2023 1 19) 0, 3)
              ]
          )
          (UTCTime (fromGregorian 2023 1 18) 0),
    testCase "at the last change" $
      3
        @=? peek @UTCTime @Int
          ( Timeline
              1
              [ (UTCTime (fromGregorian 2023 1 16) 0, 2),
                (UTCTime (fromGregorian 2023 1 19) 0, 3)
              ]
          )
          (UTCTime (fromGregorian 2023 1 19) 0),
    testCase "after all changes" $
      3
        @=? peek @UTCTime @Int
          ( Timeline
              1
              [ (UTCTime (fromGregorian 2023 1 16) 0, 2),
                (UTCTime (fromGregorian 2023 1 19) 0, 3)
              ]
          )
          (UTCTime (fromGregorian 2023 1 20) 0)
  ]

-- The purpose of the first testProperty is to verify this rewrite rule
-- suggested by hlint is legal. (We are testing if the Applicative instance is correct)
{-# ANN test_apply ("HLint: ignore Use <$>" :: String) #-}
test_apply :: [TestTree]
test_apply =
  [ testProperty "pure f <*> x === f <$> x" $
      property $ do
        timeline <- forAll $ gen genUTCTime (Gen.int (Range.linear 0 1000))
        fmap (+ 1) timeline === (pure (+ 1) <*> timeline),
    testProperty "combined timeline" $
      property $ do
        t1 <- forAll $ gen genUTCTime (Gen.int (Range.linear 0 100))
        t2 <- forAll $ gen genUTCTime (Gen.int (Range.linear (-100) 0))
        let combined = liftA2 (+) t1 t2
        -- check the size
        changes t1 `Set.union` changes t2 === changes combined
        -- for random time
        time <- forAll genUTCTime
        peek t1 time + peek t2 time === peek combined time
        -- for the times that changes happen
        let timepoints = Set.toList $ changes combined
        zipWith (+) (fmap (peek t1) timepoints) (fmap (peek t2) timepoints) === fmap (peek combined) timepoints
  ]

test_imap :: [TestTree]
test_imap =
  [ testProperty "when ignoring the range, it works the same as fmap" $ property $ do
      tl <- forAll $ gen genUTCTime (Gen.int (Range.constant 0 1000))
      imap (const (+ 1)) tl === fmap (+ 1) tl,
    testCase "check the time ranges" $ do
      let t1 = UTCTime (fromGregorian 2023 1 16) 0
          t2 = UTCTime (fromGregorian 2023 1 19) 0
          timeline =
            Timeline @UTCTime @Int
              1
              [ (t1, 2),
                (t2, 3)
              ]
          result = execWriter . sequenceA $ imap (\range _ -> tell @[TimeRange UTCTime] [range]) timeline
      result
        @?= [ TimeRange Nothing (Just t1),
              TimeRange (Just t1) (Just (addUTCTime (secondsToNominalDiffTime 259200) t1)),
              TimeRange (Just t2) Nothing
            ],
    testProperty "law: imap f . imap g === imap (\\i -> f i . g i)" $ property $ do
      tl <- forAll $ gen genUTCTime (Gen.int (Range.constant 0 1000))
      let hashTimeRange :: TimeRange UTCTime -> Int
          hashTimeRange TimeRange {from, to} = hash (show from) `hashWithSalt` show to
          f :: TimeRange UTCTime -> Int -> Int
          f tr x = hashTimeRange tr + x
          g :: TimeRange UTCTime -> Int -> Int
          g tr x = hashTimeRange tr - x
      (imap f . imap g) tl === imap (\i -> f i . g i) tl,
    testProperty "law: imap (\\_ a -> a) === id" $ property $ do
      tl <- forAll $ gen genUTCTime (Gen.int (Range.constant 0 1000))
      imap (\_ a -> a) tl === tl
  ]

buildGoldenTest :: (a -> Text) -> TestName -> a -> TestTree
buildGoldenTest pretty name value =
  goldenVsString
    name
    ("test/golden/" <> fmap (\ch -> if ch == ' ' then '_' else ch) name <> ".txt")
    $ pure . LBS.fromStrict . T.encodeUtf8 . pretty
    $ value

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
