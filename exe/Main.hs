{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Module for custom catalog format
import           Catalog

-- Modules required for IO with Pandoc
import qualified Data.Text.IO as TIO
import           Text.Pandoc

-- Placeholder maine routine, meant for testing expoort to pandoc's Markdown.
main :: IO ()
main =
    let rExtenstions = def{ readerExtensions = pandocExtensions }
        wExtenstions = def{ writerExtensions = pandocExtensions }
    in TIO.getContents
       >>= runIOorExplode . (readCatalog rExtenstions)
       >>= runIOorExplode . (writeCatalog wExtenstions)
       >>= TIO.putStrLn

