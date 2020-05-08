{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Aeson where

import Data.Aeson
import Data.Aeson.Types
import Data.Validity
import Data.Validity.HashMap ()
import Data.Validity.Scientific ()
import Data.Validity.Text ()
import Data.Validity.Vector ()

-- | A 'Value' is valid if the recursive components are valid.
instance Validity Value where
  validate (Object o) = annotate o "Object"
  validate (Array a) = annotate a "Array"
  validate (String t) = annotate t "String"
  validate (Number s) = annotate s "Number"
  validate (Bool b) = annotate b "Bool"
  validate Null = valid

-- | Modify a parser to fail on invalid results.
parseJSONValid :: Validity a => Parser a -> Parser a
parseJSONValid p = do
  r <- p
  case prettyValidate r of
    Left err -> fail $ unwords ["Parsed value is not valid:", err]
    Right r' -> pure r'

-- | Modify a parsing function to fail on invalid results.
--
-- Easy to use with the `withX` helper functions:
--
-- > parseJSON = parseJSONValidWith . withObject "MyThing" $ \o ->
-- >   MyThing <$> ...
parseJSONValidWith :: Validity a => (value -> Parser a) -> (value -> Parser a)
parseJSONValidWith func v = parseJSONValid $ func v
