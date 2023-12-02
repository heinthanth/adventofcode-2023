module AOC2_1 where

import Data.List (tail)
import Data.Text (Text, isInfixOf, pack, splitOn, unpack)
import Data.Void (Void)
import Flow ((|>))

redBallCount = 12

greenBallCount = 13

blueBallCount = 14

validateBallCount :: Text -> Bool
validateBallCount ballString =
  (redBallFromGame <= redBallCount)
    && (greenBallFromGame <= greenBallCount)
    && (blueBallFromGame <= blueBallCount)
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

validateBalls :: [Text] -> Bool
validateBalls balls =
  map validateBallCount balls
    |> filter not
    |> length
    |> (== 0)

parseBallCount :: String -> [[Text]]
parseBallCount game =
  splitOn (pack ": ") (pack game)
    |> last
    |> splitOn (pack "; ")
    |> map (splitOn (pack ", "))

validateGame :: String -> Int
validateGame gameInput =
  splitOn (pack "\n") (pack gameInput)
    |> map unpack
    |> filter (/= "")
    |> map parseBallCount
    |> map (all validateBalls)
    |> zip [1 ..]
    |> map (\x -> if snd x then fst x else 0)
    |> sum

aoc2_1_Main :: IO ()
aoc2_1_Main = do
  document <- getContents
  print (validateGame document)
