{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-duplicate-exports #-}

-- | 'GenValid' instances for 'URI' and 'URIAuth'
--
-- The main API of this module is in the orphan instances @GenValid URI@ and @GenValid URIAuth@.
module Data.GenValidity.URI
  ( -- ** Specific Generators
    genURIReference,
    genURI,
    genRelativeReference,
    genAbsoluteURI,

    -- * Export everything for testing, **You probably do not want to use any of the functions below**.
    module Data.GenValidity.URI,
  )
where

import Data.Char as Char
import Data.GenValidity
import Data.IP
import Data.Validity.URI
import Data.Word
import Network.URI
import Test.QuickCheck
import Text.Printf

-- | Generate an 'URI' that parses using 'parseURIReference'.
genURIReference :: Gen URI
genURIReference =
  oneof
    [ genURI,
      genRelativeReference
    ]

-- | Generate an 'URI' that parses using 'parseURI'.
genURI :: Gen URI
genURI = (`suchThatMap` (parseURI . dangerousURIToString)) $ do
  uriScheme <- genScheme `suchThat` (not . null)
  uriAuthority <- genValid
  uriPath <- case uriAuthority of
    Just _ -> genPathAbEmpty
    Nothing ->
      oneof
        [ genPathAbsolute,
          genPathNoScheme,
          genPathEmpty
        ]
  uriQuery <- genQuery
  uriFragment <- genFragment
  pure URI {..}

-- | Generate an 'URI' that parses using 'parseRelativeReference'.
genRelativeReference :: Gen URI
genRelativeReference = (`suchThatMap` (parseRelativeReference . dangerousURIToString)) $ do
  let uriScheme = ""
  uriAuthority <- genValid
  uriPath <- case uriAuthority of
    Just _ -> genPathAbEmpty
    Nothing ->
      oneof
        [ genPathAbsolute,
          genPathNoScheme,
          genPathEmpty
        ]
  uriQuery <- genQuery
  uriFragment <- genFragment
  pure URI {..}

-- | Generate an 'URI' that parses using 'parseAbsoluteURI'.
genAbsoluteURI :: Gen URI
genAbsoluteURI = (`suchThatMap` (parseAbsoluteURI . dangerousURIToString)) $ do
  uriScheme <- genScheme `suchThat` (not . null)
  uriAuthority <- genValid
  uriPath <- case uriAuthority of
    Just _ -> genPathAbEmpty
    Nothing ->
      oneof
        [ genPathAbsolute,
          genPathNoScheme,
          genPathEmpty
        ]
  uriQuery <- genQuery
  let uriFragment = ""
  pure URI {..}

instance GenValid URIAuth where
  genValid = (`suchThat` isValid) $ do
    uriUserInfo <- genUserInfo
    uriRegName <- genHost
    uriPort <- genPort
    pure $ URIAuth {..}
  shrinkValid (URIAuth ui rn p) = filter isValid $ do
    ((ui', rn'), p') <- shrinkTuple (shrinkTuple shrinkUserInfo shrinkHost) shrinkPort ((ui, rn), p)
    pure (URIAuth ui' rn' p')

instance GenValid URI where
  genValid = (`suchThat` isValid) . (`suchThatMap` (parseURIReference . dangerousURIToString)) $ do
    uriScheme <- genScheme
    uriAuthority <- genValid
    uriPath <- genPath
    uriQuery <- genQuery
    uriFragment <- genFragment
    pure $ URI {..}
  shrinkValid (URI s mAuth p q f) = filter isValid $ do
    (((s', mAuth'), (p', q')), f') <-
      shrinkTuple
        ( shrinkTuple
            (shrinkTuple shrinkScheme shrinkValid)
            (shrinkTuple shrinkPath shrinkQuery)
        )
        shrinkFragment
        (((s, mAuth), (p, q)), f)
    pure (URI s' mAuth' p' q' f')

genScheme :: Gen String
genScheme =
  oneof
    [ pure "",
      nullOrAppend ':'
        <$> ( (:)
                <$> genCharALPHA
                <*> genStringBy
                  ( frequency
                      [ (4, genCharALPHA),
                        (3, genCharDIGIT),
                        (1, elements ['+', '-', '.'])
                      ]
                  )
            ),
      -- Common schemes
      elements
        [ "http:",
          "https:",
          "ftp:",
          "ftps:",
          "ldap:",
          "mailto:",
          "news:",
          "tel:",
          "telnet:",
          "urn:",
          "ws:",
          "wss:"
        ]
    ]

-- Shrinking this is complicated, so we only shrink to more secure versions and to "no scheme"
shrinkScheme :: String -> [String]
shrinkScheme = \case
  "" -> []
  "http:" -> ["", "https:"]
  "ftp:" -> ["", "ftps:"]
  "ws:" -> ["", "wss:"]
  _ -> [""]

genUserInfo :: Gen String
genUserInfo =
  nullOrAppend '@' . concat
    <$> genListOf
      ( frequency
          [ (4, (: []) <$> genCharUnreserved),
            (1, genPercentEncodedChar),
            (1, (: []) <$> genCharSubDelim),
            (1, pure ":")
          ]
      )

-- Shrinking this is quite complex, so we only try to shrink to no user info.
shrinkUserInfo :: String -> [String]
shrinkUserInfo = \case
  "" -> []
  _ -> [""]

genHost :: Gen String
genHost =
  oneof
    [ genIPLiteral,
      genIPv4Address,
      genRegName
    ]

-- Shrinking this is quite complex, so we only try to shrink to localhost
shrinkHost :: String -> [String]
shrinkHost = \case
  "localhost" -> []
  _ -> ["localhost"]

genIPLiteral :: Gen String
genIPLiteral = do
  a <-
    oneof
      [ genIPv6Address,
        genIPvFuture
      ]
  pure $ "[" ++ a ++ "]"

genIPv6Address :: Gen String
genIPv6Address = show . toIPv6w <$> genValid

genIPvFuture :: Gen String
genIPvFuture = do
  hd <- genStringBy1 genCharHEXDIG
  s <-
    genStringBy1
      ( frequency
          [ (4, genCharUnreserved),
            (1, genCharSubDelim),
            (1, pure ':')
          ]
      )
  pure $ concat ["v", hd, ".", s]

genIPv4Address :: Gen String
genIPv4Address = show . toIPv4w <$> genValid

genRegName :: Gen String
genRegName =
  concat
    <$> genListOf
      ( frequency
          [ (4, (: []) <$> genCharUnreserved),
            (1, genPercentEncodedChar),
            (1, (: []) <$> genCharSubDelim)
          ]
      )

genPort :: Gen String
genPort = nullOrPrepend ':' <$> genStringBy genCharDIGIT

-- Shrinking this requires parsing the port, which we don't care about, so we
-- only try to shrink to "no port".
shrinkPort :: String -> [String]
shrinkPort = \case
  "" -> []
  _ -> [""]

genPercentEncodedChar :: Gen String
genPercentEncodedChar = do
  octet <- choose (0, 255)
  pure $ '%' : printf "%x" (octet :: Word8)

genPath :: Gen String
genPath =
  -- @
  -- path          = path-abempty    ; begins with "/" or is empty
  --               / path-absolute   ; begins with "/" but not "//"
  --               / path-noscheme   ; begins with a non-colon segment
  --               / path-rootless   ; begins with a segment
  --               / path-empty      ; zero characters
  -- @
  oneof
    [ genPathAbEmpty,
      genPathAbsolute,
      genPathNoScheme,
      genPathRootless,
      genPathEmpty
    ]

-- @
-- path-abempty  = *( "/" segment )
-- @
genPathAbEmpty :: Gen String
genPathAbEmpty = do
  segments <- genListOf genSegment
  pure $ concatMap ('/' :) segments

-- @
-- path-absolute = "/" [ segment-nz *( "/" segment ) ]
-- @
genPathAbsolute :: Gen String
genPathAbsolute = do
  firstSegment <- genSegmentNz
  restSegments <- genListOf genSegment
  pure $ '/' : firstSegment ++ concatMap ('/' :) restSegments

-- @
-- path-noscheme = segment-nz-nc *( "/" segment )
-- @
genPathNoScheme :: Gen String
genPathNoScheme = do
  firstSegment <- genSegmentNzNc
  restSegments <- genListOf genSegment
  pure $ firstSegment ++ concatMap ('/' :) restSegments

-- @
-- path-rootless = segment-nz *( "/" segment )
-- @
genPathRootless :: Gen String
genPathRootless = do
  firstSegment <- genSegmentNz
  restSegments <- genListOf genSegment
  pure $ firstSegment ++ concatMap ('/' :) restSegments

-- @
-- path-empty    = 0<pchar>
-- @
genPathEmpty :: Gen String
genPathEmpty = pure ""

-- @
-- segment       = *pchar
-- @
genSegment :: Gen String
genSegment = concat <$> genListOf genPathChar

-- @
-- segment-nz    = 1*pchar
-- @
genSegmentNz :: Gen String
genSegmentNz = concat <$> genListOf1 genPathChar

-- @
-- segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
--               ; non-zero-length segment without any colon ":"
-- @
genSegmentNzNc :: Gen String
genSegmentNzNc =
  concat
    <$> genListOf1
      ( frequency
          [ (4, (: []) <$> genCharUnreserved),
            (1, genPercentEncodedChar),
            (1, (: []) <$> genCharSubDelim),
            (1, pure "@")
          ]
      )

-- @
-- pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
-- @
genPathChar :: Gen String
genPathChar =
  frequency
    [ (4, (: []) <$> genCharUnreserved),
      (1, genPercentEncodedChar),
      (1, (: []) <$> genCharSubDelim),
      (1, elements [":", "@"])
    ]

-- Shrinking this is complicated so we only shrink to "no path"
shrinkPath :: String -> [String]
shrinkPath = \case
  "" -> []
  _ -> [""]

-- @
-- query       = *( pchar / "/" / "?" )
-- @
genQuery :: Gen String
genQuery =
  nullOrPrepend '?' . concat
    <$> genListOf
      ( frequency
          [ (4, genPathChar),
            ( 1,
              elements
                [ "/",
                  "?",
                  "&" -- '&' is not specified but very common in queries so we add it here.
                ]
            )
          ]
      )

-- | Shrinking this is complicated so we only shrink to "no query".
shrinkQuery :: String -> [String]
shrinkQuery = \case
  "" -> []
  _ -> [""]

-- @
-- fragment    = *( pchar / "/" / "?" )
-- @
genFragment :: Gen String
genFragment =
  nullOrPrepend '#' . concat
    <$> genListOf
      ( frequency
          [ (4, genPathChar),
            (1, elements ["/", "?"])
          ]
      )

-- | Shrinking this is complicated so we only shrink to "no fragment".
shrinkFragment :: String -> [String]
shrinkFragment = \case
  "" -> []
  _ -> [""]

genCharUnreserved :: Gen Char
genCharUnreserved =
  frequency
    [ (1, elements ['+', '-', '.', '~']),
      (4, genCharALPHA),
      (3, genCharDIGIT)
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

nullOrAppend :: Char -> String -> String
nullOrAppend c s = if null s then s else s ++ [c]

nullOrPrepend :: Char -> String -> String
nullOrPrepend c s = if null s then s else c : s
