import Data.List (nub, sort, sortBy)

main = do
  contents <- readFile "input.txt"
  let lineLst = lines contents
  let numLstLst = map (map (read :: String -> Int) . words) lineLst
  let safeNumLstLst = filter isSafe numLstLst

  print (length safeNumLstLst)

differences :: (Num a) => [a] -> [a]
differences xs = zipWith (-) (tail xs) xs

stalinSort :: (Ord a) => [a] -> [a]
stalinSort [] = []
stalinSort (x : xs) = stalinSort' x xs

stalinSort' :: (Ord a) => a -> [a] -> [a]
stalinSort' prev [] = [prev]
stalinSort' prev (x : xs)
  | x > prev = prev : stalinSort' x xs
  | otherwise = stalinSort' prev xs

reverseIfHeadGreaterThanLast :: (Num a, Ord a) => [a] -> [a]
reverseIfHeadGreaterThanLast xs = if head xs > last xs then reverse xs else xs

isSafe :: (Num a, Ord a) => [a] -> Bool
isSafe xs =
  let filtered = reverseIfHeadGreaterThanLast xs
      sorted = stalinSort filtered
      absDiffs = map abs (differences sorted)
      filterDiffs = filter (<= 3) absDiffs
      lenFiltered = length filterDiffs
   in lenFiltered == length xs - 1 || lenFiltered == length xs - 2
