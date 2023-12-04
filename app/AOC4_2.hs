module AOC4_2 where

import Data.List (intersect, sort)
import Data.Text (pack, replace, splitOn, unpack)
import Debug.Trace
import Flow ((|>))

calculateGamePoint :: String -> Int
calculateGamePoint game =
  length (availableNums `intersect` winningNums)
  where
    winningNums =
      splitOn (pack "|") (pack game)
        |> head
        |> splitOn (pack " ")
        |> filter (/= pack "")
        |> map unpack
        |> map read ::
        [Int]
    availableNums =
      splitOn (pack "|") (pack game)
        |> last
        |> splitOn (pack " ")
        |> filter (/= pack "")
        |> map unpack
        |> map read ::
        [Int]

calculatePoint :: String -> [Int]
calculatePoint input =
  splitOn (pack "\n") (pack input)
    |> map unpack
    |> filter (/= "")
    |> map pack
    |> map (\x -> splitOn (pack ":") x |> last |> unpack)
    |> map calculateGamePoint

funRecursivething :: [(Int, Int)] -> Int -> Int -> [Int]
funRecursivething cardsMatches matchesCount index =
  case cardsMatches of
    [] -> []
    matches ->
      newCardMatches
        |> take matchesCount
        |> zip [0 ..]
        |> map (\(idx, x) -> fst x : funRecursivething newCardMatches (snd x) idx)
        |> concat
      where
        newCardMatches = matches |> drop (index + 1)

processCardMatches :: [(Int, Int)] -> [[Int]]
processCardMatches cardMatches =
  cardMatches
    |> zip [0 ..]
    |> map (\(idx, x) -> fst x : funRecursivething cardMatches (snd x) idx)

aoc4_2_Main :: IO ()
aoc4_2_Main = do
  input <- getContents
  print
    ( calculatePoint input
        |> zip [1 ..]
        |> processCardMatches
        |> concat
        |> sort
        |> length
    )