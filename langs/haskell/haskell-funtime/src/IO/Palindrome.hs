module IO.Palindrome where

import           Control.Monad
import           System.IO
import           Data.Char


main :: IO ()
main = do
    -- rt <- mapM_ print [1..3]
    --print rt

  interact palindrome

  withFile'
    "test.txt"
    ReadMode
    (\handle -> do
      contents <- hGetContents handle
      putStr contents
    )

  contents <- readFile "test.txt"
  putStr contents

  writeFile "test2.txt" (map toUpper contents)

  appendFile "test2.txt" "hi\n"

  withFile'
    "test.txt"
    ReadMode
    (\handle -> do
      hSetBuffering handle $ BlockBuffering (Just 2048)
      contents <- hGetContents handle
      putStr contents
    )

    --interact $ unlines . filter ((<10) . length) . lines

    --c <- getChar
    --when (c /= ' ') $ do
    --    putChar c
    --    main

    --putStrLn "Hello World!" >> getLine >>= (\x -> putStrLn ("Hey" ++ x))


palindrome :: String -> String
palindrome =
  unlines . map (\x -> if x == reverse x then "yes" else "no") . lines


withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result
