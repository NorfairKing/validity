{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.URI where

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
        declare "The user info is empty or ends in @" $
          -- Laziness prevents the partial 'last' from blowing up.
          null uriUserInfo || last uriUserInfo == '@',
        declare (unwords ["The port", show uriPort, "is empty or starts with ':'"]) $
          -- Laziness prevents the partial 'head' from blowing up.
          null uriPort || head uriPort == ':',
        declare (unwords ["The port", show uriPort, "looks like a port"]) $ case uriPort of
          [] -> True
          (':' : rest) -> isJust (readMaybe rest :: Maybe Word16)
          _ -> False
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
        declare (unwords ["The scheme", show uriScheme, "is empty or ends in ':'"]) $
          -- Laziness prevents the partial 'last' from blowing up.
          null uriScheme || last uriScheme == ':',
        -- We could also not check this, because the library parses some URIs wrong (https://github.com/haskell/network-uri/issues/76)
        -- But we opted to do "the right thing" and ignore the library's bugs here.
        declare (unwords ["The path", show uriPath, "is empty or starts with '/'"]) $
          -- Laziness prevents the partial 'head' from blowing up.
          null uriPath || head uriPath == '/',
        declare (unwords ["The query", show uriQuery, "is empty or starts with '?'"]) $
          -- Laziness prevents the partial 'head' from blowing up.
          null uriQuery || head uriQuery == '?',
        declare (unwords ["The fragment", show uriFragment, "is empty or starts with '#'"]) $
          -- Laziness prevents the partial 'head' from blowing up.
          null uriFragment || head uriFragment == '#'
      ]

-- This uses 'uriToString id' as the docs specify.
-- It potentially exposes passwords, so only use it if you know what you're
-- doing.
unsafeURIToString :: URI -> String
unsafeURIToString u = uriToString id u ""

-- unsafeURIAuthToString :: URIAuth -> String
-- unsafeURIAuthToString ua = uriAuthToString id (Just ua) ""
