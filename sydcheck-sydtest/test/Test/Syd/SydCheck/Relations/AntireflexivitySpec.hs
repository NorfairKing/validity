module Test.Syd.SydCheck.Relations.AntireflexivitySpec (spec) where

import Test.Syd
import Test.Syd.SydCheck

spec :: Spec
spec = do
  expectPassing $
    specify "(<) is antireflexive for Ints" $
      antireflexivity ((<) :: Int -> Int -> Bool)
  expectFailing $
    specify "(==) is not antireflexive for Ints" $
      antireflexivity ((==) :: Int -> Int -> Bool)
