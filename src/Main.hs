{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Data.String.Conversions
import System.Directory
import System.FilePath
import Text.Show.Pretty

import qualified Data.ByteString.Char8 as SBS

import Game
import Gcc
import GccExercises
import SECD
import Lang

main :: IO ()
main = do
    setCurrentDirectory "../scheme"
    getDirectoryContents "." >>= mapM_ demoScmToGcc . sort . filter ((== ".scm") . takeExtension)

demoScmToGcc :: FilePath -> IO ()
demoScmToGcc file = do
    putStrLn $ "\n\n*** " ++ file ++ " ***\n\n"
    scheme_sample :: SBS <- SBS.readFile file
    SBS.putStrLn scheme_sample
    schemeToSECD scheme_sample >>= putStrLn . Data.String.Conversions.cs
    schemeToGcc scheme_sample >>= \ (Right prog) -> putStrLn . codeGen $ instList prog
