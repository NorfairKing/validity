{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-duplicate-exports #-}

-- [RFC 3986, section 3](https://datatracker.ietf.org/doc/html/rfc3986#section-3)
module Data.Validity.URI
  ( dangerousURIToString,
    -- Export everything for testing
    module Data.Validity.URI,
  )
where

import Data.Char as Char
import Data.Validity
import Network.URI

instance Validity URIAuth where
  validate ua@URIAuth {..} =
    mconcat
      [ genericValidate ua,
        validateUserInfo uriUserInfo,
        validateHost uriRegName,
        validatePort uriPort
      ]

instance Validity URI where
  validate u@URI {..} =
    mconcat
      [ genericValidate u,
        let rendered = dangerousURIToString u
            parsed = parseURIReference rendered
            explanation =
              unlines
                [ "Roundtrips through a parse",
                  "rendered:",
                  rendered,
                  "parsed:",
                  show $ dangerousURIToString <$> parsed
                ]
         in declare explanation $
              case parseURIReference rendered of
                Nothing -> False
                Just u' -> u' == u,
        validateScheme uriScheme,
        validatePath uriPath,
        validateQuery uriQuery,
        validateFragment uriFragment
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
        null uriUserInfo || last uriUserInfo == '@',
      case uriUserInfo of
        [] -> valid
        _ ->
          decorateString
            -- init is safe because of the case above
            (init uriUserInfo)
            validateUserInfoChar
    ]

-- [RFC 3986, section 3.2.1](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.1)
--
-- @
-- userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
-- @
validateUserInfoChar :: Char -> Validation
validateUserInfoChar c =
  declare "The character is unreserved, part of a percent-encoding, a sub-delimiter, or ':'" $
    case c of
      ':' -> True
      _ ->
        charIsUnreserved c
          ||
          -- NOTE:
          -- Technically this is not good enough, because incorrectly-percent-encoded values should be disallowed.
          -- However, this is good enough because we do the extra parsing elsewhere
          charIsPossiblyPartOfPercentEncoding c
          || charIsSubDelim c

-- [RFC 3986, section 3.2.2](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.2)
-- @
-- host        = IP-literal / IPv4address / reg-name
-- @
--
-- @
-- IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
--
-- IPvFuture  = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
-- @
--
-- @
-- reg-name    = *( unreserved / pct-encoded / sub-delims )
-- @
validateHost :: String -> Validation
validateHost s =
  declare "The host looks like an IP literal, an IPv4 Address, or a reg-name" $
    stringIsIPLiteral s
      || isIPv4address s
      || stringIsRegName s

stringIsIPLiteral :: String -> Bool
stringIsIPLiteral =
  -- NOTE this is technically not good enough but it is made up for in other parts of the validation.
  const True

stringIsRegName :: String -> Bool
stringIsRegName = all isRegNameChar

isRegNameChar :: Char -> Bool
isRegNameChar c =
  charIsUnreserved c
    ||
    -- NOTE:
    -- Technically this is not good enough, because incorrectly-percent-encoded values should be disallowed.
    -- However, this is good enough because we do the extra parsing elsewhere
    charIsPossiblyPartOfPercentEncoding c
    || charIsSubDelim c

-- [RFC 3986, section 3.2.3](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.3)
-- @
-- port        = *DIGIT
-- @
--
-- NOTE: The spec does not specify a maximum length of ports so we do not validate this either.
validatePort :: String -> Validation
validatePort uriPort =
  mconcat
    [ case uriPort of
        [] -> valid
        [':'] -> invalid "The port must not just be a ':'"
        (':' : rest) -> decorateString rest validatePortChar
        _ -> invalid "The port is empty or starts with ':'"
    ]

-- [RFC 3986, section 3.2.3](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.3)
-- @
-- port        = *DIGIT
-- @
validatePortChar :: Char -> Validation
validatePortChar c = declare "The character is a digit" $ charIsDIGIT c

-- [RFC 3986, section 2.2](https://datatracker.ietf.org/doc/html/rfc3986#section-2.2)
-- @
-- unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
-- @
charIsUnreserved :: Char -> Bool
charIsUnreserved = \case
  '+' -> True
  '-' -> True
  '.' -> True
  '~' -> True
  c -> charIsALPHA c || charIsDIGIT c

-- [RFC 3986, section 2.2](https://datatracker.ietf.org/doc/html/rfc3986#section-2.2)
-- @
-- reserved      = gen-delims / sub-delims
-- @
charIsReserved :: Char -> Bool
charIsReserved c = charIsGenDelim c || charIsSubDelim c

-- [RFC 3986, section 2.2](https://datatracker.ietf.org/doc/html/rfc3986#section-2.2)
-- @
-- gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
-- @
charIsGenDelim :: Char -> Bool
charIsGenDelim = \case
  ':' -> True
  '/' -> True
  '?' -> True
  '#' -> True
  '[' -> True
  ']' -> True
  '@' -> True
  _ -> False

-- [RFC 3986, section 2.2](https://datatracker.ietf.org/doc/html/rfc3986#section-2.2)
-- @
-- sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
--             / "*" / "+" / "," / ";" / "="
-- @
charIsSubDelim :: Char -> Bool
charIsSubDelim = \case
  '!' -> True
  '$' -> True
  '&' -> True
  '\'' -> True
  '(' -> True
  ')' -> True
  '*' -> True
  '+' -> True
  ',' -> True
  ';' -> True
  '=' -> True
  _ -> False

-- [RFC 3986, section 2.1](https://datatracker.ietf.org/doc/html/rfc3986#section-2.1)
-- @
-- pct-encoded = "%" HEXDIG HEXDIG
-- @
charIsPossiblyPartOfPercentEncoding :: Char -> Bool
charIsPossiblyPartOfPercentEncoding = \case
  '%' -> True
  c -> charIsHEXDIG c

-- [RFC 3986, section 2.3](https://datatracker.ietf.org/doc/html/rfc3986#section-2.3)
-- @
-- ALPHA (%41-%5A and %61-%7A)
-- @
charIsALPHA :: Char -> Bool
charIsALPHA c =
  let o = Char.ord c
   in (0x41 <= o && o <= 0x5A)
        || (0x61 <= o && o <= 0x7A)

-- [RFC 3986, section 2.3](https://datatracker.ietf.org/doc/html/rfc3986#section-2.3)
-- @
-- The uppercase hexadecimal digits 'A' through 'F' are equivalent to
-- the lowercase digits 'a' through 'f', respectively.  If two URIs
-- differ only in the case of hexadecimal digits used in percent-encoded
-- octets, they are equivalent.
-- @
charIsHEXDIG :: Char -> Bool
charIsHEXDIG c =
  charIsDIGIT c
    || ('A' <= c && c <= 'F')
    || ('a' <= c && c <= 'f')

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

-- | Render a URI to a 'String', for use in testing
--
-- This uses 'uriToString id' as the docs specify.
-- It potentially exposes passwords, so only use it if you know what you're
-- doing.
dangerousURIToString :: URI -> String
dangerousURIToString u = uriToString id u ""
