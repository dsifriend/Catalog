{-# LANGUAGE OverloadedStrings #-}
module Catalog where

import Data.List.Split
import qualified Data.Text as T
import Text.Pandoc
import Text.Pandoc.Builder

-- The data structures described in the basic catalog format
type Attribute = (,) T.Text T.Text
data Entry = Entry T.Text [Attribute] deriving (Eq, Show)

-- Conversion from abstract Entry struct to an equivalent in Pandoc's AST
formattedEntry :: Entry -> Blocks
formattedEntry (Entry title attributes) =
    header 2 (str title) <>
    (definitionList $ formattedAttribute <$> attributes)

-- Conversion from abstract Attribute struct to an equivalent in Pandoc's AST
formattedAttribute :: Attribute -> (Inlines, [Blocks])
formattedAttribute (att, content) =
    (strong (str att), [para (str content)])

-- Builds a catalog in Pandoc form from the contents of a text
-- readCatalog :: PandocMonad m => ReaderOptions -> T.Text -> m Pandoc
-- readCatalog opts s =
--     -- This parser function defines the catalog format in practice.
--     let parse     [] = Entry T.empty []
--         parse    [x] = Entry       x []
--         parse (x:xs) = Entry       x (keyValSplit . T.words <$> xs)
--                                 where keyValSplit [] = (T.empty, T.empty)
--                                       keyValSplit (w:ws) = (w, T.unwords ws)
--     in do
--         parsed <- parse <$> (splitWhen (== T.empty) (T.lines s))
--         return $ doc (mconcat $ formattedEntry <$> parsed)

-- Builds a catalog in Pandoc form from catalog entries and an added title
readCatalog :: PandocMonad m => ReaderOptions -> T.Text -> m Pandoc
readCatalog opts s =
    return $ doc (mconcat $ formattedEntry <$> (parseCatalog s))

-- Conversion from a plaintext catalog to an abstract List of Entries
parseCatalog :: T.Text -> [Entry]
parseCatalog contents =
    let parse     [] = Entry T.empty []
        parse    [x] = Entry       x []
        parse (x:xs) = Entry       x (keyValSplit . T.words <$> xs)
                                where keyValSplit [] = (T.empty, T.empty)
                                      keyValSplit (w:ws) = (w, T.unwords ws)
    in parse <$> (splitWhen (== T.empty) (T.lines contents))


