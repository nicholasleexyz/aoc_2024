{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import System.Environment
import Data.List (transpose, sort)

main :: IO ()
main = do
  [arg] <- getArgs
  input <- fetchInput arg
  print $ solve1 input
  print $ solve2 input

fetchInput :: FilePath -> IO String
fetchInput file = do
  readFile file

solve1 :: String -> Int
solve1 =
  sum .
  map (abs . foldr ((-) . read) 0) .
  transpose .
  map sort .
  transpose .
  map words . lines

solve2 :: String -> Int
solve2 =
  (\[left, right] -> sum $ map (\a -> length (filter (== a) right) * a) left) .
  transpose .
  map (map read . words) . lines
