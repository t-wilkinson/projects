{-# Language BlockArguments #-}
{-# Language ScopedTypeVariables #-}
{-# Language PartialTypeSignatures #-}
{-# Language TypeApplications #-}

import Control.Applicative
import Control.Concurrent
import Data.Function
import Data.Maybe (fromMaybe)
import Data.Version hiding (Version)
import System.CPUTime
import System.Console.GetOpt
import System.Environment
import System.IO
import System.IO.Error
import System.Info
import System.Mem
import System.Timeout
import System.Exit
import Text.Printf
import Data.Foldable
import Control.Exception
import Foreign.Ptr
import Data.Char
import Control.Monad

main = do
    -- setAllocationCounter 500000 ; enableAllocationLimit ; performGC ; disableAllocationLimit
    setEnv "VAR" "myvar"; getEnv "VAR" >>= print; unsetEnv "VAR"; lookupEnv "VAR" >>= print
    print =<< liftA3 (,,) getArgs  getProgName  getExecutablePath
    withProgName "mynewprogname" $ getProgName >>= print
    parseArgs
    print . (!! 1) =<< getEnvironment
    -- exitSuccess ; exitWith (ExitFailure 1) ; die "hard"
    print $ (,,,)  os  arch  compilerName (showVersion compilerVersion)
    -- print =<< timeout 10000 (threadDelay 1000 *> pure "finished on time")
    -- print =<< timeout 10000 (threadDelay 100000 *> pure "Will not finish in time.")
    print cpuTimePrecision ; print =<< getCPUTime
    io

io = do
    Left e <- try $ openFile "DNE" ReadMode
    hPutStrLn stdout $ show $ isDoesNotExistError e

    h <- openFile "test.txt" ReadWriteMode
    hSetBuffering h $ LineBuffering
    hFileSize h >>= putStrLn . ("File size is " <>) . show
    hSetFileSize h 378
    -- pos <- hGetPosn h ; print pos; hGetLine h >>= putStrLn; hGetPosn h >>= print; hSetPosn pos; hGetPosn h >>= print
    hSeek h SeekFromEnd (-300)
    hGetPosn h >>= print
    hTell h >>= print
    hGetLine h >>= putStrLn
    hSetBuffering h $ BlockBuffering $ Just 100
    hShow h >>= putStrLn
    -- hSetBinaryMode h True
    -- line <- hGetLine h
    -- print line
    hClose h

    (path, h) <- openTempFileWithDefaultPermissions "/tmp" "my-tmp-file.yolo"
    hPutStrLn h path
    hClose h
    print localeEncoding
    -- readLn @Int >>= print
    -- hReady stdin >>= print
    -- hWaitForInput stdin (-1) >>= print
    -- hLookAhead stdin >>= print
    -- hGetChar stdin >>= putChar
    -- hGetChar stdin >>= putChar
    -- hGetContents stdin >>= print
    -- interact (\x -> toUpper <$> x)

    -- writeFile "test1.txt" "hey there bud"
    -- appendFile "test1.txt" "\nalright"
    -- readFile "test1.txt" >>= traverse_ putStrLn . lines

data Flag = Verbose  | Version | Input String | Output String | LibDir String
    deriving Show

parseArgs = withArgs ["-v", "-ofile", "--libdir=/asdf"] do
     let outp = Output . fromMaybe "stdout"
         inp  = Input  . fromMaybe "stdin"
         options :: [OptDescr Flag] =
             [ Option ['v']     ["verbose"] (NoArg Verbose)       "chatty output on stderr"
             , Option ['V','?'] ["version"] (NoArg Version)       "show version number"
             , Option ['o']     ["output"]  (OptArg outp "FILE")  "output FILE"
             , Option ['c']     []          (OptArg inp  "FILE")  "input FILE"
             , Option ['L']     ["libdir"]  (ReqArg LibDir "DIR") "library directory"
             ]

     argv <- getArgs
     let args = getOpt RequireOrder options argv
     print args
     putStrLn $ usageInfo "" options
     pure ()
