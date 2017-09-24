#!/usr/bin/env stack
{- stack
    --install-ghc
    runghc
    --package zifter
    --package zifter-cabal
    --package zifter-git
    --package zifter-hlint
    --package zifter-stack
-}
import Zifter
import Zifter.Cabal
import Zifter.Git
import Zifter.Hlint
import Zifter.Stack

main :: IO ()
main =
    ziftWith $ do
        recursiveZift
        preprocessor cabalFormatZift
        prechecker gitAddAllZift
        checker $ do
            hlintZift
            stackBuildZift
