{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- [RFC 3986, section 3](https://datatracker.ietf.org/doc/html/rfc3986#section-3)
module Data.Validity.URI where

import Data.Char as Char
import Data.Maybe
import Data.Validity
import Data.Word
import Network.URI
import Text.Read

instance Validity URIAuth where
  validate ua@URIAuth {..} =
    mconcat
      [ genericValidate ua,
        declare "looks rectified" $
          rectifyAuth ua == ua,
        validateUserInfo uriUserInfo,
        validatePort uriPort
      ]

instance Validity URI where
  validate u@URI {..} =
    mconcat
      [ genericValidate u,
        declare "Looks valid according to the library" $
          isURIReference (unsafeURIToString u),
        declare "Roundtrips through a parse" $
          case parseURI (unsafeURIToString u) of
            Nothing -> False
            Just u' -> u' == u,
        declare "The URI looks rectified" $
          rectify u == u,
        validateScheme uriScheme,
        validatePath uriPath,
        validateQuery uriQuery,
        validateFragment uriFragment
      ]

-- [RFC 3986, section 3.2.1](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.1)
--
-- @
-- userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
-- @
validateUserInfo :: String -> Validation
validateUserInfo uriUserInfo =
  mconcat
    [ declare "The user info is empty or ends in @" $
        -- Laziness prevents the partial 'last' from blowing up.
        null uriUserInfo || last uriUserInfo == '@'
    ]

validatePort :: String -> Validation
validatePort uriPort =
  mconcat
    [ declare (unwords ["The port", show uriPort, "is empty or starts with ':'"]) $
        -- Laziness prevents the partial 'head' from blowing up.
        null uriPort || head uriPort == ':',
      declare (unwords ["The port", show uriPort, "looks like a port"]) $ case uriPort of
        [] -> True
        (':' : rest) -> isJust (readMaybe rest :: Maybe Word16)
        _ -> False
    ]

-- [RFC 3986, section 3.1](https://datatracker.ietf.org/doc/html/rfc3986#section-3.1)
-- @
-- scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
-- @
validateScheme :: String -> Validation
validateScheme uriScheme =
  mconcat
    [ declare (unwords ["The scheme", show uriScheme, "is empty or ends in ':'"]) $
        -- Laziness prevents the partial 'last' from blowing up.
        null uriScheme || last uriScheme == ':',
      case uriScheme of
        [] -> valid
        [c] -> declare "The first character is ALPHA" (charIsALPHA c)
        (c : rest) ->
          mconcat
            [ declare "The first character is ALPHA" (charIsALPHA c),
              decorateString
                -- 'init' is safe because of case match above
                (init rest)
                validateSchemeChar
            ]
    ]

-- [RFC 3986, section 3.1](https://datatracker.ietf.org/doc/html/rfc3986#section-3.1)
-- @
-- scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
-- @
validateSchemeChar :: Char -> Validation
validateSchemeChar c =
  declare "The character is alphanumeric, '+', '-' or '.'." $
    case c of
      '+' -> True
      '-' -> True
      '.' -> True
      _ -> charIsALPHA c || charIsDIGIT c

--  [RFC 3986, section 2.3](https://datatracker.ietf.org/doc/html/rfc3986#section-2.3)
-- @
-- ALPHA (%41-%5A and %61-%7A)
-- @
charIsALPHA :: Char -> Bool
charIsALPHA c =
  let o = Char.ord c
   in (0x41 <= o && o <= 0x5A)
        || (0x61 <= o && o <= 0x7A)

--  [RFC 3986, section 2.3](https://datatracker.ietf.org/doc/html/rfc3986#section-2.3)
-- @
-- DIGIT (%30-%39)
-- @
charIsDIGIT :: Char -> Bool
charIsDIGIT c =
  let o = Char.ord c
   in (0x30 <= o && o <= 0x39)

validatePath :: String -> Validation
validatePath _uriPath =
  mconcat
    [ -- We could check the following, but it actually does not hold, see also:
      -- https://github.com/haskell/network-uri/issues/76
      --
      -- declare (unwords ["The path", show uriPath, "is empty or starts with '/'"]) $
      --   -- Laziness prevents the partial 'head' from blowing up.
      --   null uriPath || head uriPath == '/',

      valid
    ]

validateQuery :: String -> Validation
validateQuery uriQuery =
  declare (unwords ["The query", show uriQuery, "is empty or starts with '?'"]) $
    -- Laziness prevents the partial 'head' from blowing up.
    null uriQuery || head uriQuery == '?'

validateFragment :: String -> Validation
validateFragment uriFragment =
  declare (unwords ["The fragment", show uriFragment, "is empty or starts with '#'"]) $
    -- Laziness prevents the partial 'head' from blowing up.
    null uriFragment || head uriFragment == '#'

-- This uses 'uriToString id' as the docs specify.
-- It potentially exposes passwords, so only use it if you know what you're
-- doing.
unsafeURIToString :: URI -> String
unsafeURIToString u = uriToString id u ""
