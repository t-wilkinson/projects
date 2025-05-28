{-# Language
  UnicodeSyntax
, OverloadedStrings
, TypeApplications
, ScopedTypeVariables
, GADTs
, FlexibleContexts
, PartialTypeSignatures
#-}

module Main where

import Control.Monad (zipWithM, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.Foldable (asum)
import Data.Function
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.List (sortBy, partition)
import Data.Maybe (isJust, mapMaybe)
import Data.Ord (comparing)
import Data.String (fromString)
import Data.Text.Titlecase (titlecase)
import Data.Time.Clock
import Data.Time.Format
import Data.Traversable (for, traverse)
import Data.Tuple (swap)
import Data.Void (Void)
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.FilePath.Glob (compile, globDir1)
import System.FilePath.Posix (takeFileName)
import System.IO
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Byte
import qualified Data.ByteString.Lazy as BLW
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.IntMap as IM
import qualified Data.Map.Lazy as M
import qualified Data.Set as Set

type BS = BL.ByteString

main :: IO ()
main = do
    home <- getHomeDirectory
    curDir <- getCurrentDirectory
    -- let notesDir = "/notes/"
    --     mainDir = home <> notesDir
    let mainDir = curDir <> "/tmp/"
        useableWordFile = curDir <> "/usable.txt"

    filePaths <- globDir1 pat mainDir
    fileBodies <- traverse BL.readFile filePaths
    useableWords <- init . BL.split '\n' <$> BL.readFile useableWordFile

    let settings = Settings useableWords
        titlePaths = filter ((`elem` useableWords) . fst)
            $ zipBodiesPaths fileBodies filePaths
        (zettel, dneZettel') = mconcat fileBodies
            & top settings
            & filter ((>= threshold) . fst)
            & partition (\(n,t1) -> any (\(t2,_) -> t1 == t2) titlePaths)
        dneZettel = zipWith (\x (_,title) -> (x,title)) [1..] dneZettel'

    zipWithM (writeLinks curDir titlePaths) filePaths fileBodies
    pure ()

  where
    pat = compile "<->.md"  -- '<->' matches any integer
    threshold = 2
    getTitle = BL.map toLower . BL.tail . head . BL.lines
    zipBodiesPaths fileBodies filePaths = (\(body,path) -> (getTitle body, path)) <$> zip fileBodies filePaths


{- Parsing -}
type Parser a = ParsecT Void BS Identity a

writeLinks :: String -> [(BS, String)] -> String -> BS -> IO ()
writeLinks curDir titlePaths path fileBody = do
    nvimSwps <- globDir1 (compile "*") nvimSwp
    let isNvimSwp = toNvimSwp path `elem` nvimSwps
    unless isNvimSwp $ do
        let Right res = runParser parser "file" fileBody
        BL.writeFile (curDir <> "/tmp/" <> takeFileName path) $ mconcat res
    pure ()

  where
    toLink path title = "["<>title<>"]("<>(BL.pack $ takeFileName path)<>")"
    titlePathLinks = (\(title,path) -> (title, path, toLink path)) <$> titlePaths

    nvimSwp = "~/.local/share/nvim/swap"
    toNvimSwp path = nvimSwp <> fmap (\c -> if c == '/' then '%' else c) path <> ".swp"

    parser :: Parser [BS]
    parser = many (replaceLinks titlePathLinks) <* eof

replaceLinks :: [(BS, String, BS -> BS)] -> Parser BS
replaceLinks tpl      = try (pLinks tpl) <|> pName tpl <|> token (Just . BLW.singleton) Set.empty

pLink :: BS -> String -> Parser BS
pLink title path      = between (string' "[") (string' "]") (string' title) *> between (string' "(") (string' ")") (string' $ BL.pack path)

pLinks,pName :: [(BS, String, BS -> BS)] -> Parser BS
pLinks titlePathLinks = asum $ fmap (\(title,path,_) -> pLink title path) titlePathLinks
pName titlePathLinks  = asum $ fmap (\(title,_,link) -> link <$> string' title ) titlePathLinks
{-# INLINE replaceLinks #-}
{-# INLINE pLink #-}
{-# INLINE pLinks #-}
{-# INLINE pName #-}


{- Counting word occurrences -}
data Settings = Settings
    { setWords :: [BS]
    } deriving (Show)

top :: Settings -> BS -> [(Int,BS)]
top (Settings setWords) = order . buildFreq setWords

buildFreq :: [BS] -> BS -> M.Map BS Int
buildFreq setWords = count . filter (`elem` setWords) . BL.splitWith (`BL.elem` splits) . BL.map toLower
  where
    splits = " \n;:,./`-(){}[]" :: BS
{-# INLINE buildFreq #-}

count :: [BS] -> M.Map BS Int
count = M.fromListWith (+) . (`zip` repeat 1)
{-# INLINE count #-}

order :: M.Map BS Int -> [(Int, BS)]
order = sortBy (flip (comparing fst)) . fmap swap . M.toList

-- type ParserIO a = ParsecT Void BS IO a
-- testParser :: String -> [(BS, String)] -> String -> BS -> IO ()
-- testParser curDir titlePaths path fileBody = do
--     withFile (filePath path) WriteMode $ \h -> do
--         Right res' <- runParserT (parser h) "file" fileBody
--         pure ()
--     pure ()
--   where
--     filePath path = curDir <> "/tmp/" <> takeFileName path
--     toLink path title = "["<>title<>"]("<>(BL.pack $ takeFileName path)<>")"
--     titlePathLinks = (\(title,path) -> (title, path, toLink path)) <$> titlePaths

--     parser h = many (try pLinks <|> pName h <|> token (Just . BLW.singleton) Set.empty) <* eof
--     pLinks :: ParserIO BS
--     pLinks = asum $ fmap (\(title,path,_) -> pLink title path) titlePathLinks
--     pLink :: BS -> String -> ParserIO BS
--     pLink title path = between (string' "[") (string' "]") (string' title) *> between (string' "(") (string' ")") (string' $ BL.pack path)
--     pName :: Handle -> ParserIO BS
--     pName h = asum $ fmap (\(title,_,link) -> link <$> string' title ) titlePathLinks



    -- ∀zettel. getFilePath(zettel)
    -- print $ mapMaybe (\(n,title) -> do
    --     path <- lookup title titlePaths
    --     pure (title, takeFileName path)) zettel
    -- writeNewZettels curDir dneZettel

-- writeNewZettels :: String -> [(Integer, BS)] -> IO ()
-- writeNewZettels curDir dneZettel = do
--     curTime <- getCurrentTime
--     let offset = secondsToNominalDiffTime 1
--     for dneZettel $ \(x,title) -> do
--         let fileTime = formatTime defaultTimeLocale "%Y%W%u%H%M%S" $ addUTCTime (offset * fromIntegral x) curTime
--             fileName = curDir <> "/tmp/" <> fileTime <> ".md"
--         writeFile fileName $ "#" <> titlecase (BL.unpack title)
--     pure ()
