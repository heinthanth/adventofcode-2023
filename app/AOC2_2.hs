module AOC2_2 where

import Data.List (tail)
import Data.Text (Text, isInfixOf, pack, splitOn, unpack)
import Data.Void (Void)
import Flow ((|>))

redBallCount = 12

greenBallCount = 13

blueBallCount = 14

getBallCount :: Text -> [Int]
getBallCount ballString =
  [redBallFromGame, greenBallFromGame, blueBallFromGame]
  where
    redBallFromGame =
      if pack "red" `isInfixOf` ballString
        then splitOn (pack " ") ballString |> head |> unpack |> read :: Int
        else 0
    greenBallFromGame =
      if pack "green" `isInfixOf` ballString
        then splitOn (pack " ") ballString |> head |> unpack |> read :: Int
        else 0
    blueBallFromGame =
      if pack "blue" `isInfixOf` ballString
        then splitOn (pack " ") ballString |> head |> unpack |> read :: Int
        else 0

getPowerOfSet :: [[Text]] -> Int
getPowerOfSet gameEntry =
  maxRedBall * maxGreenBall * maxBlueBall
  where
    maxRedBall =
      gameEntry
        |> map (map getBallCount)
        |> concat
        |> map head
        |> maximum
    maxGreenBall =
      gameEntry
        |> map (map getBallCount)
        |> concat
        |> map (!! 1)
        |> maximum
    maxBlueBall =
      gameEntry
        |> map (map getBallCount)
        |> concat
        |> map (!! 2)
        |> maximum

parseBallCount :: String -> [[Text]]
parseBallCount game =
  splitOn (pack ": ") (pack game)
    |> last
    |> splitOn (pack "; ")
    |> map (splitOn (pack ", "))

getSumOfPowerOfSet :: String -> Int
getSumOfPowerOfSet gameInput =
  splitOn (pack "\n") (pack gameInput)
    |> map unpack
    |> filter (/= "")
    |> map parseBallCount
    |> map getPowerOfSet
    |> sum

aoc2_2_Main :: IO ()
aoc2_2_Main = do
  document <- getContents
  print (getSumOfPowerOfSet document)
