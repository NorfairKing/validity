{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.URI where

import Control.Monad
import Data.Char
import Data.GenValidity
import Data.List
import Data.Validity.URI ()
import Data.Word
import Network.URI
import Test.QuickCheck

genURIChar :: Gen Char
genURIChar =
  (chr <$> choose (0, 127)) `suchThat` isAllowedInURI

genURIString :: Gen String
genURIString = genListOf genURIChar

genURIComponentString :: Gen String
genURIComponentString = escapeURIString isUnescapedInURIComponent <$> genListOf genURIChar

genURIStringSeparatedBy :: Char -> Gen String
genURIStringSeparatedBy c = do
  ll <- (`div` 5) . max 1 <$> genListLength
  intercalate [c] <$> replicateM ll (escapeURIString isUnescapedInURIComponent <$> genURIComponentString)

instance GenValid URI where
  genValid = (`suchThat` isValid) $ do
    uriScheme <- nullOrAppend ':' <$> genURIComponentString

    uriAuthority <- genValid

    uriPath <- nullOrPrepend '/' <$> genURIStringSeparatedBy '/'

    uriQuery <- nullOrAppend '?' <$> genURIStringSeparatedBy '&'

    uriFragment <- nullOrPrepend '#' <$> genURIComponentString

    pure $ rectify URI {..}

instance GenValid URIAuth where
  genValid = (`suchThat` isValid) $ do
    uriUserInfo <- nullOrAppend '@' <$> genURIComponentString

    uriRegName <- genURIStringSeparatedBy '.'

    port <- genValid :: Gen Word16
    let uriPort = ':' : show port

    pure $ rectifyAuth URIAuth {..}

nullOrAppend :: Char -> String -> String
nullOrAppend c s = if null s then s else s ++ [c]

nullOrPrepend :: Char -> String -> String
nullOrPrepend c s = if null s then s else c : s
