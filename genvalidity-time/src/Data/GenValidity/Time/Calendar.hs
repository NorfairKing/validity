{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Time.Calendar where

import Data.GenValidity
import Data.Time.Calendar
import Data.Validity.Time.Calendar ()
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Test.QuickCheck

instance GenUnchecked Day where
  genUnchecked = ModifiedJulianDay <$> genUnchecked
  shrinkUnchecked (ModifiedJulianDay i) = ModifiedJulianDay <$> shrinkUnchecked i

instance GenValid Day where
  genValid =
#if MIN_VERSION_time(1,10,0)
      let fancy = oneof
            [ (ModifiedJulianDay <$> genValid) `suchThat` isValid
            , uniformlyOneHundredYearsAround2020
            ]
       in fancy
#else
      uniformlyOneHundredYearsAround2020
#endif
    where
      uniformlyOneHundredYearsAround2020 = do
        y <- choose (1970, 2070)
        m <- choose (1, 12)
        d <- choose (1, 31)
        pure $ fromGregorian y m d
  shrinkValid (ModifiedJulianDay i) = ModifiedJulianDay <$> shrinkValid i

genSmartDayAround :: Day -> Gen Day
genSmartDayAround d = oneof [genValid , genDayAround d, genDayCloselyAround d]

-- We cannot put this in 'GenValid Day' around 'today' because that would break reproducability
genDayAround :: Day -> Gen Day
genDayAround today = do
  let (thisYear, _, _) =  toGregorian today
  y <- choose (pred thisYear, succ thisYear)
  m <- choose (1, 12)
  d <- choose (1, 31)
  pure $ fromGregorian y m d

genDayCloselyAround :: Day -> Gen Day
genDayCloselyAround today = sized $ \s -> do
  diff <- choose (-s, s)
  pure $ addDays (fromIntegral diff) today

#if MIN_VERSION_time(1,9,0)
instance GenUnchecked CalendarDiffDays where
  genUnchecked = CalendarDiffDays <$> genUnchecked <*> genUnchecked
  shrinkUnchecked (CalendarDiffDays m d) = [ CalendarDiffDays m' d' | (m', d') <- shrinkUnchecked (m, d) ]

instance GenValid CalendarDiffDays where
  genValid = CalendarDiffDays <$> genValid <*> genValid
  shrinkValid (CalendarDiffDays m d) = [ CalendarDiffDays m' d' | (m', d') <- shrinkValid (m, d) ]

instance GenUnchecked DayOfWeek where
  genUnchecked = elements
    [ Monday
    , Tuesday
    , Wednesday
    , Thursday
    , Friday
    , Saturday
    , Sunday
    ]
  -- It's hard to know how to shrink this, because there is no official start of the week.
  -- However, just as we would shrink MonthOfYear to January, we will shrink the days of the week to monday
  shrinkUnchecked Monday = []
  shrinkUnchecked Tuesday = [Monday]
  shrinkUnchecked Wednesday = [Monday, Tuesday]
  shrinkUnchecked Thursday = [Monday, Tuesday, Wednesday]
  shrinkUnchecked Friday = [Monday, Tuesday, Wednesday, Thursday]
  shrinkUnchecked Saturday = [Monday, Tuesday, Wednesday, Thursday, Friday]
  shrinkUnchecked Sunday = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]

instance GenValid DayOfWeek where
  genValid = genUnchecked
  shrinkValid = shrinkUnchecked
#endif
