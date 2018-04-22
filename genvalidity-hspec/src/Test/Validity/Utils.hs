{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

-- | Utilities for defining your own validity 'Spec's
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.Utils
    ( nameOf
    , genDescr
    , binRelStr
    , shouldFail
    , failsBecause
    , Anon(..)
    , shouldBeValid
    , shouldBeInvalid
    ) where

import Data.Data
import Data.Validity

import Control.Monad

import Test.Hspec
import Test.Hspec.Core.Formatters
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Spec
import Test.QuickCheck.Property

nameOf ::
       forall a. Typeable a
    => String
nameOf = show $ typeRep (Proxy @a)

genDescr ::
       forall a. Typeable a
    => String
    -> String
genDescr genname = unwords ["\"" ++ genname, "::", nameOf @a ++ "\""]

binRelStr ::
       forall a. Typeable a
    => String
    -> String
binRelStr op = unwords ["(" ++ op ++ ")", "::", name, "->", name, "->", "Bool"]
  where
    name = nameOf @a

newtype Anon a =
    Anon a

instance Show (Anon a) where
    show _ = "Anonymous"

instance Functor Anon where
    fmap f (Anon a) = Anon (f a)

-- | Asserts that a given 'Spec' tree fails _somewhere_.
--
-- It also shows the given string when reporting that the tree unexpectedly
-- succeeded.
failsBecause :: String -> SpecWith () -> SpecWith ()
failsBecause s = mapSpecTree go
  where
    go :: SpecTree () -> SpecTree ()
    go sp =
        Leaf
            Item
                { itemRequirement = s
                , itemLocation = Nothing
                , itemIsParallelizable = False
                , itemExample =
                      \_ _ _ -> do
                          let conf =
                                  defaultConfig {configFormatter = Just silent}
                          r <- hspecWithResult conf $ fromSpecList [sp]
                          let succesful =
                                  summaryExamples r > 0 && summaryFailures r > 0
                          pure $ produceResult succesful
                }
#if MIN_VERSION_hspec_core(2,4,0)
produceResult :: Bool -> Either a Test.Hspec.Core.Spec.Result
produceResult succesful =
    Right $
    if succesful
        then Success
        else Failure Nothing $ Reason "Should have failed but didn't."
#else
produceResult :: Bool -> Test.Hspec.Core.Spec.Result
produceResult succesful =
    if succesful
        then Success
        else Fail Nothing "Should have failed but didn't."
#endif
shouldFail :: Property -> Property
shouldFail =
    mapResult $ \res ->
        res
            { reason = unwords ["Should have failed:", reason res]
            , expect = not $ expect res
            }

shouldBeValid :: (Show a, Validity a) => a -> Expectation
shouldBeValid a = do
    case prettyValidation a of
        Right _ -> pure ()
        Left err ->
            expectationFailure $
            unlines
                [ "'validate' reported this value to be invalid: " ++ show a
                , err
                , ""
                ]
    unless (isValid a) $
        expectationFailure $
        unlines
            [ "isValid considered this value invalid: " ++ show a
            , "This is odd because 'validate' reported no issues."
            , "Are you sure 'Validity' is implemented correctly?"
            ]

shouldBeInvalid :: (Show a, Validity a) => a -> Expectation
shouldBeInvalid a = a `shouldNotSatisfy` isValid
