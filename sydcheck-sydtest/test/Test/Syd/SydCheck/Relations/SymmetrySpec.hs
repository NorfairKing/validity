module Test.Syd.SydCheck.Relations.SymmetrySpec (spec) where

import Test.Syd
import Test.Syd.SydCheck

spec :: Spec
spec = do
  expectPassing $
    specify "(==) is symmetric for Ints" $
      symmetry ((==) :: Int -> Int -> Bool)
  specify "(<) is not symmetric for Ints" $
    symmetry ((<) :: Int -> Int -> Bool)
