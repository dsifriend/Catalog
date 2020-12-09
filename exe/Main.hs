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
    txt <- TIO.readFile "examples/in.txt"
    let catalogDoc = (catalog "Catalog") . readCatalog $ txt
    let withExtenstions =
            def{ writerExtensions = pandocExtensions }
    rst <- runIO (writeMarkdown withExtenstions catalogDoc) >>= handleError
    TIO.putStrLn rst

