module Data.GenValidity.Text where

import           Data.GenValidity

import           Test.QuickCheck

import           Data.Text        (Text)
import qualified Data.Text        as T


genUncheckedText :: Gen Text
genUncheckedText = sized $ \n -> do
    size <- upTo n
    chars <- resize size $ genListOf arbitrary
    return $ T.pack chars

textStartingWith :: Char -> Gen Text
textStartingWith c = sized $ \n ->
    case n of
        0 -> pure $ T.singleton c
        1 -> pure $ T.singleton c
        _ -> do
            rest <- resize (n - 1) genUncheckedText
            return $ T.cons c rest

textWith :: Gen Text -> Gen Text
textWith gen = sized $ \n -> do
    (b, m, a) <- genSplit3 n
    before <- resize b genUncheckedText
    middle <- resize m gen
    after  <- resize a genUncheckedText
    return $ T.concat [before, middle, after]

textWithA :: Char -> Gen Text
textWithA c = textWith $ T.singleton <$> pure c

textWithoutAny :: Char -> Gen Text
textWithoutAny c = textWithoutAnyOf [c]

textWithoutAnyOf :: [Char] -> Gen Text
textWithoutAnyOf cs = T.pack <$> genListOf (arbitrary `suchThat` (`notElem` cs))

textAllCaps :: Gen Text
textAllCaps = T.toUpper <$> genUncheckedText





