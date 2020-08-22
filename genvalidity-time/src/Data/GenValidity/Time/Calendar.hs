{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Time.Calendar where

import Data.GenValidity
import Data.Time.Calendar
import System.IO.Unsafe
import Data.Time
import Data.Validity.Time.Calendar ()
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Test.QuickCheck

instance GenUnchecked Day where
  genUnchecked = ModifiedJulianDay <$> genUnchecked
  shrinkUnchecked _ = []

instance GenValid Day where
  genValid = oneof
    [ (ModifiedJulianDay <$> genValid) `suchThat` isValid
    , uniformlyOneHundredYearsAroundToday
    , uniformlyThreeYearsAroundToday
    , uniformlyDaysAroundToday
    ]

    where
      uniformlyOneHundredYearsAroundToday = do
        y <- choose (1970, 2070)
        m <- choose (1, 12)
        d <- choose (1, 31)
        pure $ fromGregorian y m d

      uniformlyThreeYearsAroundToday = do
        let (thisYear, _, _) =  toGregorian today
        y <- choose (pred thisYear, succ thisYear)
        m <- choose (1, 12)
        d <- choose (1, 31)
        pure $ fromGregorian y m d

      uniformlyDaysAroundToday = sized $ \s -> do
        diff <- choose (0, s)
        pure $ addDays (fromIntegral diff) today

      -- This will make the code potentially flaky, but that's an okay tradeoff given that it
      -- will generate days around the current day: A positive and negative diff around today.
      today :: Day
      today = unsafePerformIO $ utctDay <$> getCurrentTime
