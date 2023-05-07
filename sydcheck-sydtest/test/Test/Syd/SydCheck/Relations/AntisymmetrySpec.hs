module Test.Syd.SydCheck.Relations.AntisymmetrySpec (spec) where

import Test.Syd
import Test.Syd.SydCheck

spec :: Spec
spec = do
  expectPassing $
    specify "(<) is antisymmetric for Ints" $
      antisymmetry ((<) :: Int -> Int -> Bool)
