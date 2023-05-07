module Test.Syd.SydCheck.Relations.TransitivitySpec (spec) where

import Test.Syd
import Test.Syd.SydCheck

spec :: Spec
spec = do
  expectPassing $
    specify "(<) is transitive for Ints" $
      transitivity ((<) :: Int -> Int -> Bool)
  expectFailing $
    modifyMaxSuccess (* 10) $
      specify "(/=) is not transitive for Word8s" $
        transitivity ((/=) :: Int -> Int -> Bool)
