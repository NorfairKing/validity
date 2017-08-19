import Test.DocTest

import Control.Monad

import System.Directory
import System.FilePath

main :: IO ()
main = do
    fs <- listFilesRecur "src"
    doctest $ "-isrc" : fs

listFilesRecur :: FilePath -> IO [FilePath]
listFilesRecur dir = do
    entries <-
            (map (dir </>) .
             filter (\f -> f /= "." && f /= ".." && takeExtension f /= ".swp")) <$>
        getDirectoryContents dir
    dirs <- filterM doesDirectoryExist entries
    files <- filterM doesFileExist entries
    rests <- mapM listFilesRecur dirs
    return $ concat $ files : rests
