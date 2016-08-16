module Data.GenValidity.Text where

import           Data.GenValidity
import           Data.Validity.Text ()

import           Test.QuickCheck

import           Control.Monad

import qualified Data.Text.Array    as A
import           Data.Text.Internal (Text(..))
import qualified Data.Text          as T


instance GenValidity Text where
    genUnchecked = Text <$> uncheckedArray <*> arbitrary <*> arbitrary
      where
        uncheckedArray = sized $ \n -> do
            size <- upTo n
            ins <- replicateM size arbitrary
            return $ A.run $ do
              arr <- A.new size
              forM_ (zip [0..] ins) $ \(ix, word) ->
                A.unsafeWrite arr ix word
              return arr

    genValid = sized $ \n -> do
        size <- upTo n
        chars <- resize size $ genListOf arbitrary
        return $ T.pack chars

textStartingWith :: Char -> Gen Text
textStartingWith c = sized $ \n ->
    case n of
        0 -> pure $ T.singleton c
        1 -> pure $ T.singleton c
        _ -> do
            rest <- resize (n - 1) genValid
            return $ T.cons c rest

textWith :: Gen Text -> Gen Text
textWith gen = sized $ \n -> do
    (b, m, a) <- genSplit3 n
    before <- resize b genValid
    middle <- resize m gen
    after  <- resize a genValid
    return $ T.concat [before, middle, after]

textWithA :: Char -> Gen Text
textWithA c = textWith $ T.singleton <$> pure c

textWithoutAny :: Char -> Gen Text
textWithoutAny c = textWithoutAnyOf [c]

textWithoutAnyOf :: [Char] -> Gen Text
textWithoutAnyOf cs = T.pack <$> genListOf (arbitrary `suchThat` (`notElem` cs))

textAllCaps :: Gen Text
textAllCaps = T.toUpper <$> genValid





