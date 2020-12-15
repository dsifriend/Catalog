{-# LANGUAGE OverloadedStrings #-}
module Catalog where

import           Data.List.Split
import           Data.Maybe
import qualified Data.Text           as T
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.Shared

-- The data structures described in the basic catalog format
type Attribute = (,) T.Text T.Text
data Entry = Entry T.Text [Attribute] deriving (Eq, Show)

empty :: Entry
empty = Entry T.empty []

-- Builds a catalog in Pandoc form from the contents of a text. Ignores opts
readCatalog :: PandocMonad m => ReaderOptions -> T.Text -> m Pandoc
readCatalog _ s =
    -- This parser function defines the catalog format in practice.
    let parse     [] = Entry "" []
        parse    [x] = Entry  x []
        parse (x:xs) = Entry  x (keyValSplit . T.words <$> xs)
                           where keyValSplit []     = ("", "")
                                 keyValSplit (w:ws) = (w, T.unwords ws)
    in (return . doc . mconcat . catMaybes
       . (formattedEntry . parse <$>) . splitWhen ("" ==) . T.lines) s

-- Conversion from abstract Entry struct to an equivalent in Pandoc's AST
formattedEntry :: Entry -> Maybe Blocks
formattedEntry entry =
    let formatted (Entry title attributes) =
            header 2 (str title) <>
            definitionList (formattedAttribute <$> attributes)
    in if entry /= empty then Just (formatted entry) else Nothing

-- Conversion from abstract Attribute struct to an equivalent in Pandoc's AST
formattedAttribute :: Attribute -> (Inlines, [Blocks])
formattedAttribute (att, content) =
    (strong (str att), [para (str content)])

-- Generates a Catalog from a Pandoc document. Ignores opts, might fail.
writeCatalog :: PandocMonad m => WriterOptions -> Pandoc -> m T.Text
writeCatalog _ (Pandoc _ bs) =
    let evalInline a = case a of
                     Str x    -> x
                     Strong x -> T.intercalate " " (evalInline <$> x)
                     _        -> ""
        evalBlock a = case a of
                    Para xs           -> T.intercalate "\n" (evalInline <$> xs)
                    Header 2 _ xs     -> T.intercalate " " (evalInline <$> xs)
                    DefinitionList xs -> T.intercalate "\n"
                                            ((\(x,y) -> "\t" <> x <> " " <> y)
                                            . extractAtt <$> xs)
                                            <> "\n"
                    _ -> ""
    in (return . T.intercalate "\n" . (evalBlock <$>)) bs

-- Conversion from Pandoc 'Definition" to Catalog atribute.
extractAtt :: ([Inline], [[Block]]) -> Attribute
extractAtt (x,y) =
    (stringify x, (stringify . blocksToInlinesWithSep (str "\n") . concat) y)

