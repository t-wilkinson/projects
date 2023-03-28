module IO where

import           Data.Function
import           System.Random
import           Control.Monad

import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString               as S

import           System.Environment
import           System.IO
import           System.IO.Error
import           Control.Exception


main :: IO ()
main = do
  toTry `catch` handler
  --contents <- getContents
  -- putStrLn contents
  mapM_ (putStr . show) . reverse $ [1 .. 3]
  putStrLn ""
  forM_ [1 .. 3] print
  --rs <- sequence [getLine, getLine]
  --print rs
  --when (rs /= ["1","2"]) $ putStrLn "rs /= [1,2]"

  "hi" & (print >=> print)
  putStr $ take 30 $ randomRs ('A', 'z') (mkStdGen 100)
  --B.pack [200..300]
  --B.unpack $ B.pack [99,97,100]
  --B.fromChunks [S.pack [100],S.pack [100,200]]
  --B.cons' 90 $ B.cons' 100 $ B.cons 200 $ B.pack [100]
 where
  shorter = interact $ unlines . filter ((< 10) . length) . lines
  palindrome =
    interact
      $ unlines
      . map (\xs -> if xs == reverse xs then "palindrome" else "no")
      . lines

guessMyNumber :: IO ()
guessMyNumber = do
  gen <- getStdGen
  let (randNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
  putStr "Which number in the range from 1 to 10 am I thinking of? "
  numberString <- getLine
  unless (null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    newStdGen
    main


toTry :: IO ()
toTry = do
  (fileName : _) <- getArgs
  contents       <- readFile fileName
  putStrLn $ "This file has " ++ show (length (lines contents)) ++ " Lines!"

handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e = case ioeGetFileName e of
    Just path -> putStrLn $ "Whhops files : " ++ path
    Nothing   -> putStrLn "weird file path"
  | otherwise = ioError e
