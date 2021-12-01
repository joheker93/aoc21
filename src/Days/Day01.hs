{-# LANGUAGE TupleSections #-}
module Days.Day01 where
import Solution

solA :: Run [Int] String
solA = run id

solB :: Run [Int] String
solB = run (foldr1 (+)) . chunks 3

run :: Ord a => (b -> a) -> Run [b] String
run f = prep . fst . ((foldl (\(a,p) c -> (a + btoi (f c > f p),c))) =<< ((0,) . head))

chunks :: Num a => Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop 1 xs)

--- Parsing
parseA :: Parser [Int]
parseA = parse

parseB :: Parser [Int]
parseB = parse

parse :: Parser [Int]
parse = map stoi . words

