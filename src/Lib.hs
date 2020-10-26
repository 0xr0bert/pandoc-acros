module Lib where

import Text.Pandoc.JSON
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

type Acronyms = Map T.Text (Inline, Inline, Bool)

processPandoc :: Pandoc -> Pandoc
processPandoc (Pandoc meta blocks) = 
    let acronyms = processAcronyms meta 
    in case acronyms of
        (Just acronyms) -> Pandoc meta $ fst $ processBlocks blocks acronyms
        Nothing -> Pandoc meta blocks

processAcronyms :: Meta -> Maybe Acronyms
processAcronyms meta =
    let acros = lookupMeta (T.pack "acronyms") meta
    in case acros of
        (Just (MetaList l)) -> Just (processAcronymList l $ Map.empty)
        Nothing -> Nothing

processAcronymList :: [MetaValue] -> Acronyms -> Acronyms
processAcronymList (x:xs) acs = 
    let (in1, in2, b) = processAcronymEntry x 
    in case in1 of
        Span _ ((Str x):_) -> processAcronymList xs $ Map.insert x (in1, in2, b) acs
        _ -> processAcronymList xs acs
processAcronymList [] acs = acs

processAcronymEntry :: MetaValue -> (Inline, Inline, Bool)
processAcronymEntry (MetaMap m) = (Span nullAttr short, Span nullAttr long, False) where
    short = case Map.lookup (T.pack "short") m of
        (Just (MetaInlines ins)) -> ins
    long = case Map.lookup (T.pack "long") m of
        (Just (MetaInlines ins)) -> ins

processList :: [a] -> Acronyms -> (a -> Acronyms -> (a, Acronyms)) -> ([a], Acronyms)
processList (x:xs) m f =
    let (xProcessed, mNew) = f x m
        (xsProcessed, mNewest) = processList xs mNew f
    in (xProcessed:xsProcessed, mNewest)
processList [] m _ = ([], m)

processBlocksSquared :: [[Block]] -> Acronyms -> ([[Block]], Acronyms)
processBlocksSquared xs m = processList xs m processBlocks

processBlocks :: [Block] -> Acronyms -> ([Block], Acronyms)
processBlocks xs m = processList xs m processBlock

processBlock :: Block -> Acronyms -> (Block, Acronyms)
processBlock (Plain xs) m = let (xsNew, mNew) = processInlines xs m in ((Plain xsNew), mNew)
processBlock (Para xs) m = let (xsNew, mNew) = processInlines xs m in ((Para xsNew), mNew)
processBlock (LineBlock xs) m = let (xsNew, mNew) = processInlinesSquared xs m in ((LineBlock xsNew), mNew)
processBlock (BlockQuote xs) m = let (xsNew, mNew) = processBlocks xs m in ((BlockQuote xsNew), mNew)
processBlock (OrderedList attrs xs) m = let (xsNew, mNew) = processBlocksSquared xs m in ((OrderedList attrs xsNew), mNew)
processBlock (Header i a xs) m = let (xsNew, mNew) = processInlines xs m in ((Header i a xsNew), mNew)
processBlock (Table a b c d e f) m = processTable (Table a b c d e f) m
processBlock (Div attr xs) m = let (xsNew, mNew) = processBlocks xs m in ((Div attr xsNew), mNew)
processBlock xs m = (xs, m)

processCaption :: Caption -> Acronyms -> (Caption, Acronyms)
processCaption (Caption (Just inlines) blocks) m =
    let (inlinesProcessed, mNew) = processInlines inlines m
        (blocksProcessed, mNewest) = processBlocks blocks mNew
    in (Caption (Just inlinesProcessed) blocksProcessed, mNewest)
processCaption (Caption Nothing blocks) m =
    let (blocksProcessed, mNew) = processBlocks blocks m
    in (Caption Nothing blocksProcessed, mNew)

processCell :: Cell -> Acronyms -> (Cell, Acronyms)
processCell (Cell a b c d xs) m =
    let (xsProcessed, mNew) = processBlocks xs m
    in ((Cell a b c d xsProcessed), mNew)

processCells :: [Cell] -> Acronyms -> ([Cell], Acronyms)
processCells xs m = processList xs m processCell

processRow :: Row -> Acronyms -> (Row, Acronyms)
processRow (Row attr xs) m = let (xsProcessed, mNew) = processCells xs m in ((Row attr xsProcessed), mNew)

processRows :: [Row] -> Acronyms -> ([Row], Acronyms)
processRows xs m = processList xs m processRow

processTableHead :: TableHead -> Acronyms -> (TableHead, Acronyms)
processTableHead (TableHead attr rows) m = let (rowsProcessed, mNew) = processRows rows m in ((TableHead attr rowsProcessed), mNew)

processTableBody :: TableBody -> Acronyms -> (TableBody, Acronyms)
processTableBody (TableBody attr rhc rows1 rows2) m =
    let (rows1Processed, mNew1) = processRows rows1 m
        (rows2Processed, mNew2) = processRows rows2 mNew1
    in ((TableBody attr rhc rows1Processed rows2Processed), mNew2)

processTableBodies :: [TableBody] -> Acronyms -> ([TableBody], Acronyms)
processTableBodies xs m = processList xs m processTableBody

processTableFoot :: TableFoot -> Acronyms -> (TableFoot, Acronyms)
processTableFoot (TableFoot attr rows) m = let (rowsProcessed, mNew) = processRows rows m in ((TableFoot attr rowsProcessed), mNew)

processTable :: Block -> Acronyms -> (Block, Acronyms)
processTable (Table attr capt colspecs thead tbodies tfoot) m =
    let (captNew, mNew1) = processCaption capt m
        (theadNew, mNew2) = processTableHead thead mNew1
        (tbodiesNew, mNew3) = processTableBodies tbodies mNew2
        (tfootNew, mNew4) = processTableFoot tfoot mNew3
    in ((Table attr captNew colspecs theadNew tbodiesNew tfootNew), mNew4)

processInlinesSquared :: [[Inline]] -> Acronyms -> ([[Inline]], Acronyms)
processInlinesSquared xs m = processList xs m processInlines

processInlines :: [Inline] -> Acronyms -> ([Inline], Acronyms)
processInlines xs m = processList xs m processInline

processInline :: Inline -> Acronyms -> (Inline, Acronyms)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "ac") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createStandardAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acs") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createShortAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acf") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createFullAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acl") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createLongAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acp") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createStandardPluralAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acps") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createShortPluralAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acpf") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createFullPluralAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acpl") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createLongPluralAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createStandardAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acsstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createShortAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acfstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createFullAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "aclstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createLongAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acpstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createStandardPluralAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acpsstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createShortPluralAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acpfstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createFullPluralAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "acplstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createLongPluralAcronym (ident, cls, kvp) mapEntry
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Ac") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createStandardAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acs") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createShortAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acf") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createFullAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acl") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createLongAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acp") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createStandardPluralAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acps") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createShortPluralAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acpf") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createFullPluralAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acpl") cls = (ac, mNew) where
    mapEntry = getAcroMapEntry m x
    mNew = createUpdatedMap m x mapEntry
    ac = createLongPluralAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createStandardAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acsstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createShortAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acfstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createFullAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Aclstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createLongAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acpstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createStandardPluralAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acpsstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createShortPluralAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acpfstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createFullPluralAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Span (ident, cls, kvp) ((Str x):xs)) m | elem (T.pack "Acplstar") cls = (ac, m) where
    mapEntry = getAcroMapEntry m x
    ac = createLongPluralAcronym (ident, cls, kvp) (capitaliseMapEntry mapEntry)
processInline (Emph xs) m = let (xsNew, mNew) = processInlines xs m in ((Emph xsNew), mNew)
processInline (Underline xs) m = let (xsNew, mNew) = processInlines xs m in ((Underline xsNew), mNew)
processInline (Strong xs) m = let (xsNew, mNew) = processInlines xs m in ((Strong xsNew), mNew)
processInline (Strikeout xs) m = let (xsNew, mNew) = processInlines xs m in ((Strikeout xsNew), mNew)
processInline (Superscript xs) m = let (xsNew, mNew) = processInlines xs m in ((Superscript xsNew), mNew)
processInline (Subscript xs) m = let (xsNew, mNew) = processInlines xs m in ((Subscript xsNew), mNew)
processInline (SmallCaps xs) m = let (xsNew, mNew) = processInlines xs m in ((SmallCaps xsNew), mNew)
processInline (Quoted qtype xs) m = let (xsNew, mNew) = processInlines xs m in ((Quoted qtype xsNew), mNew)
processInline (Cite citations xs) m = let (xsNew, mNew) = processInlines xs m in ((Cite citations xsNew), mNew)
processInline (Link attr xs target) m = let (xsNew, mNew) = processInlines xs m in ((Link attr xsNew target), mNew)
processInline (Image attr xs target) m = let (xsNew, mNew) = processInlines xs m in ((Image attr xsNew target), mNew)
processInline (Note blocks) m = let (blocksNew, mNew) = processBlocks blocks m in ((Note blocksNew), mNew)
processInline (Span attr xs) m = let (xsNew, mNew) = processInlines xs m in ((Span attr xsNew), mNew)
processInline x m = (x, m)

createStandardAcronym :: Attr -> (Inline, Inline, Bool) -> Inline
createStandardAcronym attr (in1, in2, False) = createFullAcronym attr (in1, in2, False)
createStandardAcronym attr (in1, in2, True) = createShortAcronym attr (in1, in2, True)

createFullAcronym :: Attr -> (Inline, Inline, Bool) -> Inline
createFullAcronym attr (in1, in2, _) = Span attr [
        Emph [in2],
        Str $ T.pack " (",
        SmallCaps [in1],
        Str $ T.pack ")"
    ]

createShortAcronym :: Attr -> (Inline, Inline, Bool) -> Inline
createShortAcronym attr (in1, _, _) = Span attr [
        SmallCaps [in1]
    ]

createLongAcronym :: Attr -> (Inline, Inline, Bool) -> Inline
createLongAcronym attr (_, in2, _) = Span attr [
        in2
    ]

createStandardPluralAcronym :: Attr -> (Inline, Inline, Bool) -> Inline
createStandardPluralAcronym attr (in1, in2, False) = createFullPluralAcronym attr (in1, in2, False)
createStandardPluralAcronym attr (in1, in2, True) = createShortPluralAcronym attr (in1, in2, True)

createFullPluralAcronym :: Attr -> (Inline, Inline, Bool) -> Inline
createFullPluralAcronym attr (in1, in2, _) = Span attr [
        Emph [in2, (Str (T.pack "s"))],
        Str $ T.pack " (",
        SmallCaps [in1],
        Str $ T.pack "s",
        Str $ T.pack ")"
    ]

createShortPluralAcronym :: Attr -> (Inline, Inline, Bool) -> Inline
createShortPluralAcronym attr (in1, _, _) = Span attr [
        SmallCaps [in1],
        Str $ T.pack "s"
    ]

createLongPluralAcronym :: Attr -> (Inline, Inline, Bool) -> Inline
createLongPluralAcronym attr (_, in2, _) = Span attr [
        in2,
        Str $ T.pack "s"
    ]

getAcroMapEntry :: Acronyms -> T.Text -> (Inline, Inline, Bool)
getAcroMapEntry m x = maybe ((Strong [Str (T.pack "???")]), (Strong [Str (T.pack "???")]), False) id (Map.lookup x m)

createUpdatedMap :: Acronyms -> T.Text -> (Inline, Inline, Bool) -> Acronyms
createUpdatedMap m k (in1, in2, _) = Map.adjust updateFun k m where
    updateFun (in1, in2, _) = (in1, in2, True)

capitaliseFirstWord :: Inline -> Inline
capitaliseFirstWord (Span attr ((Str x):xs)) = Span attr ((Str (T.toTitle x)):xs)
capitaliseFirstWord (Str x) = Str (T.toTitle x)
capitaliseFirstWord x = x

capitaliseMapEntry :: (Inline, Inline, Bool) -> (Inline, Inline, Bool)
capitaliseMapEntry (in1, in2, b) = (in1, capitaliseFirstWord in2, b)