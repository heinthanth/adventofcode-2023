module AOC1_1 (getSumFromDocument) where

import Data.Char (isDigit)
import Data.Semigroup ((<>))
import Data.Text (Text, find, findIndex, pack, reverse, singleton, splitOn, unpack)
import Flow ((|>))

extractNumber :: Text -> Int
extractNumber line =
  read (firstNumber <> secondNumber) :: Int
  where
    firstNumber = case find isDigit line of
      Just maybeNumber -> unpack (singleton maybeNumber)
      Nothing -> ""
    secondNumber = case find isDigit (Data.Text.reverse line) of
      Just maybeNumber -> unpack (singleton maybeNumber)
      Nothing -> ""

getSumFromDocument :: String -> Int
getSumFromDocument document =
  splitOn (pack "\n") (pack document)
    |> filter (/= pack "")
    |> map extractNumber
    |> sum

aoc1_1_Main :: IO ()
aoc1_1_Main = do
  document <- getContents
  print (getSumFromDocument document)
