module Test.Syd.SydCheck.Relations.ReflexivitySpec (spec) where

import Test.Syd
import Test.Syd.SydCheck

spec :: Spec
spec = do
  expectPassing $
    specify "(<=) is reflexive for Ints" $
      reflexivity ((<=) :: Int -> Int -> Bool)
  expectFailing $
    specify "(<) is not reflexive for Ints" $
      reflexivity ((<) :: Int -> Int -> Bool)
