module AOC6_2 where

import Data.Text (intercalate, pack, splitOn, unpack)
import Debug.Trace (traceShow)
import Flow ((|>))

mapTimeAndDistance :: String -> (Int, Int)
mapTimeAndDistance gamesData =
  (timeData, distanceData)
  where
    rawData =
      gamesData
        |> pack
        |> splitOn (pack "\n")
    timeData =
      head rawData
        |> splitOn (pack " ")
        |> filter (/= pack "")
        |> drop 1
        |> intercalate (pack "")
        |> unpack
        |> read ::
        Int

    distanceData =
      last rawData
        |> splitOn (pack " ")
        |> filter (/= pack "")
        |> drop 1
        |> intercalate (pack "")
        |> unpack
        |> read ::
        Int

getDistanceForHoldPeriod :: Int -> Int -> Int
getDistanceForHoldPeriod maxTime holdTime = (maxTime - holdTime) * holdTime

getNumberOfWaysToBeat :: (Int, Int) -> Int
getNumberOfWaysToBeat (time, distance) =
  maxX - minX + 1
  where
    t = fromIntegral time :: Double
    r = fromIntegral distance :: Double
    minX = ceiling $ (t - sqrt (t ^ 2 - 4 * r)) / 2
    maxX = floor $ (t + sqrt (t ^ 2 - 4 * r)) / 2

aoc6_2_Main :: IO ()
aoc6_2_Main = do
  input <- getContents
  print (mapTimeAndDistance input |> getNumberOfWaysToBeat)