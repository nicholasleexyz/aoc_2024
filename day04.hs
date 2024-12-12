import Data.List
import System.Environment

type Matrix a = [[a]]
type Line a = [a]

diagonals :: Matrix a -> [Line a]
diagonals orig =
  transpose (zipWith drop [0 ..] orig) ++
  transpose (map reverse $ zipWith take [0 ..] orig)

allDirections :: Matrix a -> [Line a]
allDirections orig = concat [
      orig                             -- horizontals
    , transpose orig                   -- verticals
    , diagonals orig                   -- diagonals (sloping down)
    , diagonals (map reverse orig)     -- diagonals (sloping up)
  ]

-- count needle haystack
count :: Eq a => [a] -> [a] -> Int
count = (length .) . (. tails) . filter . isPrefixOf

allDirs :: Matrix a -> [Line a]
allDirs input = allDirections input ++ (map reverse . allDirections) input

part1 :: Matrix Char -> Int
part1 = sum . map (count "XMAS") . allDirs

main :: IO()
main = do
  [inputFile] <- getArgs
  input <- lines <$> readFile inputFile
  putStrLn $ "#XMAS: " ++ show (part1 input)
