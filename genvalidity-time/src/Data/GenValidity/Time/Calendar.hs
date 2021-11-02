{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Time.Calendar where

import Data.GenValidity
import Data.Time.Calendar
import Data.Validity.Time.Calendar ()
import Test.QuickCheck

instance GenValid Day where
  genValid = uniformlyOneHundredYearsAround2020
  shrinkValid (ModifiedJulianDay i) = ModifiedJulianDay <$> shrinkValid i

uniformlyOneHundredYearsAround2020 :: Gen Day
uniformlyOneHundredYearsAround2020 = do
  y <- choose (1970, 2070)
  m <- choose (1, 12)
  d <- choose (1, 31)
  pure $ fromGregorian y m d

genSmartDayAround :: Day -> Gen Day
genSmartDayAround d = oneof [genValid, genDayAround d, genDayCloselyAround d]

-- We cannot put this in 'GenValid Day' around 'today' because that would break reproducability
genDayAround :: Day -> Gen Day
genDayAround today = do
  let (thisYear, _, _) = toGregorian today
  y <- choose (pred thisYear, succ thisYear)
  m <- choose (1, 12)
  d <- choose (1, 31)
  pure $ fromGregorian y m d

genDayCloselyAround :: Day -> Gen Day
genDayCloselyAround today = sized $ \s -> do
  diff <- choose (- s, s)
  pure $ addDays (fromIntegral diff) today

instance GenValid CalendarDiffDays where
  genValid = CalendarDiffDays <$> genValid <*> genValid
  shrinkValid (CalendarDiffDays m d) = [CalendarDiffDays m' d' | (m', d') <- shrinkValid (m, d)]

instance GenValid DayOfWeek where
  genValid =
    elements
      [ Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday,
        Sunday
      ]

  -- It's hard to know how to shrink this, because there is no official start of the week.
  -- However, just as we would shrink MonthOfYear to January, we will shrink the days of the week to monday
  shrinkValid Monday = []
  shrinkValid Tuesday = [Monday]
  shrinkValid Wednesday = [Monday, Tuesday]
  shrinkValid Thursday = [Monday, Tuesday, Wednesday]
  shrinkValid Friday = [Monday, Tuesday, Wednesday, Thursday]
  shrinkValid Saturday = [Monday, Tuesday, Wednesday, Thursday, Friday]
  shrinkValid Sunday = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]
