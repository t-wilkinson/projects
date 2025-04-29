module IO.Puzzles where

import           Data.List
import           System.Environment


main :: IO ()
main = do
  contents <- getContents
  let threes     = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a, b, c] -> Section a b c) threes
      path       = optimalPath roadSystem
      pathString = concatMap (show . fst) path
      pathPrice  = sum $ map snd path
  putStrLn $ "The best path is: " ++ pathString
  putStrLn $ "The time is: " ++ show pathPrice
    -- [input] <- getArgs
    -- putStrLn $ solveRPN input


--solveRPN :: (Num a, Read a) => String -> a
solveRPN :: String -> Float
solveRPN = head . foldl foldingfunction [] . words
 where
  foldingfunction (x : y : ys) "-"          = (x - y) : ys
  foldingfunction (x : y : ys) "+"          = (x + y) : ys
  foldingfunction (x : y : ys) "*"          = (x * y) : ys
  foldingfunction (x : y : ys) "/"          = (x / y) : ys
  foldingfunction (x : y : ys) "^"          = (x ** y) : ys
  foldingfunction (x     : xs) "ln"         = log x : xs
  foldingfunction xs           "sum"        = [sum xs]
  foldingfunction xs           numberString = read numberString : xs


data Node = Node Road Road | EndNode Road
data Road = Road Int Node

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]


heathrowToLondon :: RoadSystem
heathrowToLondon =
  [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]


optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
  in  if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath


roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA          = sum $ map snd pathA
      priceB          = sum $ map snd pathB
      forwardPriceToA = priceA + a
      crossPriceToA   = priceB + b + c
      forwardPriceToB = priceB + b
      crossPriceToB   = priceA + a + c
      newPathToA      = if forwardPriceToA <= crossPriceToA
        then (A, a) : pathA
        else (C, c) : (B, b) : pathB
      newPathToB = if forwardPriceToB <= crossPriceToB
        then (B, b) : pathB
        else (C, c) : (A, a) : pathA
  in  (newPathToA, newPathToB)


groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
