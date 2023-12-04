module AOC4_1 where

import Data.Text (pack, splitOn, unpack, replace)
import Flow ((|>))
import Data.List (intersect)

calculateGamePoint :: String -> Int
calculateGamePoint game =
  case length (availableNums `intersect` winningNums) of
    0 -> 0
    1 -> 1
    other -> 2 ^ (other - 1)
  where
    winningNums = splitOn (pack "|") (pack game)
      |> head
      |> splitOn (pack " ")
      |> filter (/= pack "")
      |> map unpack
      |> map read :: [Int]
    availableNums = splitOn (pack "|") (pack game)
      |> last
      |> splitOn (pack " ")
      |> filter (/= pack "")
      |> map unpack
      |> map read :: [Int]

calculatePoint :: String -> Int
calculatePoint input =
  splitOn (pack "\n") (pack input)
    |> map unpack
    |> filter (/= "")
    |> map pack
    |> map (\x -> splitOn (pack ":") x |> last |> unpack)
    |> map calculateGamePoint
    |> sum

aoc4_1_Main :: IO ()
aoc4_1_Main = do
  input <- getContents
  print $ calculatePoint input