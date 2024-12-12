import Data.List
import System.Environment
import Control.Monad(guard)
import Data.Maybe (mapMaybe)

{- PART A -}

type Matrix a = [[a]]

type Line a = [a]

diagonals :: Matrix a -> [Line a]
diagonals orig =
  transpose (zipWith drop [0 ..] orig)
    ++ transpose (map reverse $ zipWith take [0 ..] orig)

allDirections :: Matrix a -> [Line a]
allDirections orig =
  concat
    [ orig, -- horizontals
      transpose orig, -- verticals
      diagonals orig, -- diagonals (sloping down)
      diagonals (map reverse orig) -- diagonals (sloping up)
    ]

-- count :: needle -> haystack -> result
count :: (Eq a) => [a] -> [a] -> Int
count = (length .) . (. tails) . filter . isPrefixOf

allDirs :: Matrix a -> [Line a]
allDirs input = allDirections input ++ (map reverse . allDirections) input

partA :: Matrix Char -> Int
partA = sum . map (count "XMAS") . allDirs
{- ------ -}

{- PART B -}

takeExactly :: Int -> [a] -> Maybe [a]
takeExactly n xs = let taken = take n xs in taken <$ guard (length taken == n)

submatrices :: forall a. Int -> Matrix a -> [Matrix a]
submatrices n = concatMap forStartingLine . mapMaybe (takeExactly n) . tails
    where
      forStartingLine :: Matrix a -> [Matrix a]
      forStartingLine xs = mapMaybe (takeExactly n) (tails (transpose xs))

-- Match a matix against a pattern
match :: Matrix (a -> Bool) -> Matrix a -> Bool
match pattern xs = and $ zipWith ($) (concat pattern) (concat xs)

xMasPatterns :: [Matrix (Char -> Bool)]
xMasPatterns =
  [ [ [is 'M', ignore, is 'M'],
      [ignore, is 'A', ignore],
      [is 'S', ignore, is 'S']
    ],
    [ [is 'M', ignore, is 'S'],
      [ignore, is 'A', ignore],
      [is 'M', ignore, is 'S']
    ],
    [ [is 'S', ignore, is 'M'],
      [ignore, is 'A', ignore],
      [is 'S', ignore, is 'M']
    ],
    [ [is 'S', ignore, is 'S'],
      [ignore, is 'A', ignore],
      [is 'M', ignore, is 'M']
    ]
  ]
  where
    is :: Char -> Char -> Bool
    is = (==)

    ignore :: Char -> Bool
    ignore = const True

partB :: Matrix Char -> Int
partB input = length $ filter subIsMatch (submatrices 3 input)
  where
    subIsMatch :: Matrix Char -> Bool
    subIsMatch sub = any (`match` sub) xMasPatterns
{- ------ -}

main :: IO ()
main = do
  [inputFile] <- getArgs
  input <- lines <$> readFile inputFile
  putStrLn $ "#XMAS: " ++ show (partA input)
  putStrLn $ "#X-MAS: " ++ show (partB input)
