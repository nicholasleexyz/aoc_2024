import Data.List (nub, sort, sortBy)

main = do
  contents <- readFile "input.txt"
  let lineLst = lines contents
  let numLstLst = map (map (read :: String -> Int) . words) lineLst
  let filtered = filter (\xs -> allIncreasingOrDecreasing xs && not (containsDuplicates xs) && graduallyChanging xs) numLstLst

  print (length filtered)

allIncreasingOrDecreasing :: (Eq a, Ord a) => [a] -> Bool
allIncreasingOrDecreasing xs = allDecreasing xs || allDecreasing xs

allIncreasing :: (Eq a, Ord a) => [a] -> Bool
allIncreasing xs = sort xs == xs

allDecreasing :: (Eq a, Ord a) => [a] -> Bool
allDecreasing xs = sortBy (flip compare) xs == xs

containsDuplicates :: (Eq a) => [a] -> Bool
containsDuplicates xs = length xs /= length (nub xs)

absDifferences :: (Num a) => [a] -> [a]
absDifferences xs = map abs (zipWith (-) (tail xs) xs)

graduallyChanging :: (Num a, Ord a) => [a] -> Bool
graduallyChanging xs = all (<= 3) (absDifferences xs)
