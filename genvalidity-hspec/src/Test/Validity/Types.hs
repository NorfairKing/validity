{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Types
    ( CanFail(..)
    ) where

-- | A class of types that are the result of functions that can fail
--
-- You should not use this class yourself.
class CanFail f where
    hasFailed :: f a -> Bool
    resultIfSucceeded :: f a -> Maybe a

instance CanFail Maybe where
    hasFailed Nothing = True
    hasFailed _ = False

    resultIfSucceeded Nothing = Nothing
    resultIfSucceeded (Just r) = Just r

instance CanFail (Either e) where
    hasFailed (Left _) = True
    hasFailed _ = False

    resultIfSucceeded (Left _) = Nothing
    resultIfSucceeded (Right r) = Just r

