{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.URI where

import Control.Monad
import Data.Char
import Data.Char as Char
import Data.GenValidity
import Data.IP
import Data.List
import Data.Validity.URI ()
import Data.Word
import Network.URI
import Test.QuickCheck
import Text.Printf

instance GenValid URIAuth where
  genValid = (`suchThat` isValid) $ do
    uriUserInfo <- genUserInfo

    uriRegName <- genHost

    port <- genValid :: Gen Word16
    let uriPort = ':' : show port

    pure $ URIAuth {..}

instance GenValid URI where
  genValid = (`suchThat` isValid) $ do
    uriScheme <- genScheme

    uriAuthority <- genValid

    uriPath <- nullOrPrepend '/' <$> genURIStringSeparatedBy '/'

    uriQuery <- nullOrAppend '?' <$> genURIStringSeparatedBy '&'

    uriFragment <- nullOrPrepend '#' <$> genURIComponentString

    pure $ URI {..}

genScheme :: Gen String
genScheme =
  oneof
    [ pure "",
      nullOrAppend ':'
        <$> ( (:)
                <$> genCharALPHA
                <*> genStringBy
                  ( oneof
                      [ genCharALPHA,
                        genCharDIGIT,
                        elements ['+', '-', '.']
                      ]
                  )
            )
    ]

genUserInfo :: Gen String
genUserInfo =
  nullOrAppend '@' . concat
    <$> genListOf
      ( oneof
          [ (: []) <$> genCharUnreserved,
            genPercentEncodedChar,
            (: []) <$> genCharSubDelim,
            pure ":"
          ]
      )

genHost :: Gen String
genHost = oneof [genIPLiteral, genIPv4Address, genRegName]

genIPLiteral :: Gen String
genIPLiteral = do
  a <- oneof [genIPv6Address, genIPvFuture]
  pure $ "[" ++ a ++ "]"

genIPv6Address :: Gen String
genIPv6Address = show . toIPv6w <$> genValid

genIPvFuture :: Gen String
genIPvFuture = do
  hd <- genStringBy1 genCharHEXDIG
  s <-
    genStringBy1
      ( oneof
          [ genCharUnreserved,
            genCharSubDelim,
            pure ':'
          ]
      )
  pure $ concat ["v", hd, ".", s]

genIPv4Address :: Gen String
genIPv4Address = show . toIPv4w <$> genValid

genRegName :: Gen String
genRegName =
  concat
    <$> genListOf
      ( oneof
          [ (: []) <$> genCharUnreserved,
            genPercentEncodedChar,
            (: []) <$> genCharSubDelim
          ]
      )

genPercentEncodedChar :: Gen String
genPercentEncodedChar = do
  octet <- choose (0, 255)
  pure $ '%' : printf "%x" (octet :: Word8)

genCharUnreserved :: Gen Char
genCharUnreserved =
  oneof
    [ elements ['+', '-', '.', '~'],
      genCharALPHA,
      genCharDIGIT
    ]

genCharReserved :: Gen Char
genCharReserved =
  oneof
    [ genCharGenDelim,
      genCharSubDelim
    ]

genCharGenDelim :: Gen Char
genCharGenDelim =
  elements
    [ ':',
      '/',
      '?',
      '#',
      '[',
      ']',
      '@'
    ]

genCharSubDelim :: Gen Char
genCharSubDelim =
  elements
    [ '!',
      '$',
      '&',
      '\'',
      '(',
      ')',
      '*',
      '+',
      ',',
      ';',
      '='
    ]

genCharHEXDIG :: Gen Char
genCharHEXDIG = elements $ ['1' .. '9'] ++ ['A' .. 'F'] ++ ['a' .. 'f']

genCharALPHA :: Gen Char
genCharALPHA =
  Char.chr
    <$> oneof
      [ choose (0x41, 0x5A),
        choose (0x61, 0x7A)
      ]

genCharDIGIT :: Gen Char
genCharDIGIT =
  Char.chr
    <$> choose (0x30, 0x39)

-- genURI :: Gen URI
-- genURI = undefined
--
-- genURIReference :: Gen URI
-- genURIReference = undefined
--
-- genRelativeReference :: Gen URI
-- genRelativeReference = undefined
--
-- genAbsoluteURI :: Gen URI
-- genAbsoluteURI = undefined

-- [RFC 3986 section 1.2.1](https://datatracker.ietf.org/doc/html/rfc3986#section-1.2.1)
--
-- @
-- The URI syntax has been designed with global transcription as one of
-- its main considerations.  A URI is a sequence of characters from a
-- very limited set: the letters of the basic Latin alphabet, digits,
-- and a few special characters.
-- @
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

nullOrAppend :: Char -> String -> String
nullOrAppend c s = if null s then s else s ++ [c]

nullOrPrepend :: Char -> String -> String
nullOrPrepend c s = if null s then s else c : s
