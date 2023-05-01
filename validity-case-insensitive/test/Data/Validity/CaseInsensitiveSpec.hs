module Data.Validity.CaseInsensitiveSpec
  ( spec,
  )
where

import qualified Data.CaseInsensitive as CI
import qualified Data.CaseInsensitive.Unsafe as CI
import Data.Validity.CaseInsensitive ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  it "says that this case-insensitive string is valid" $
    shouldBeValid $
      CI.mk "hello world"
  it "says that this unsafely constructed unfolded string is invalid" $
    shouldBeInvalid $
      CI.unsafeMk "HeLlO wOrLd"
