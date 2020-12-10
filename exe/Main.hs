{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Module for custom catalog format
import           Catalog

-- Modules required for IO with Pandoc
import qualified Data.Text.IO as TIO
import           Text.Pandoc

-- Placeholder maine routine, meant for testing expoort to pandoc's Markdown.
main :: IO ()
main = do
    let rExtenstions = def{ readerExtensions = pandocExtensions }
    let wExtenstions = def{ writerExtensions = pandocExtensions }
    txt <- TIO.readFile "examples/in.txt"
    catalogDoc <- runIO (readCatalog rExtenstions txt) >>= handleError
    rst <- runIO (writeMarkdown wExtenstions catalogDoc) >>= handleError
    TIO.putStrLn rst

