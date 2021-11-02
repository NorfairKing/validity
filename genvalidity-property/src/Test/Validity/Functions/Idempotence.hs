{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Standard tests involving validity
module Test.Validity.Functions.Idempotence
  ( idempotentOnGen,
    idempotent,
    idempotentOnArbitrary,
  )
where

import Data.GenValidity
import Test.Hspec
import Test.QuickCheck

idempotentOnGen :: (Show a, Eq a) => (a -> a) -> Gen a -> (a -> [a]) -> Property
idempotentOnGen f gen s = forAllShrink gen s $ \a -> f (f a) `shouldBe` f a

idempotent :: (Show a, Eq a, GenValid a) => (a -> a) -> Property
idempotent func = idempotentOnGen func genValid shrinkValid

-- |
--
-- 'id' is idempotent for any type:
--
-- prop> idempotentOnArbitrary (id :: Int -> Int)
--
-- 'const', given any input, is idempotent for any type as well:
--
-- prop> \int -> idempotentOnArbitrary (const int :: Int -> Int)
idempotentOnArbitrary :: (Show a, Eq a, Arbitrary a) => (a -> a) -> Property
idempotentOnArbitrary func = idempotentOnGen func arbitrary shrink
