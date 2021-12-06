module Days.Day06 where
import Solution
import Data.List (sum,sort,group)
import Control.Arrow ((&&&))

-- First solved with huge list explicitly, waay too slow for part B
solA :: Run [(Int,Int)] String
solA = prep . sum . map snd . run 80

solB :: Run [(Int,Int)] String
solB = prep . sum . map snd . run 256

run 0 = id
run x = run (x-1) . step

step :: [(Int,Int)] -> [(Int,Int)]
step xs = let (zs,vals) = takeWhile ((==0) . fst) &&& dropWhile ((==0) . fst) $ sort xs
              births    = sum . map snd $ zs
          in (6,births) : (8,births) : (map (dec . fst &&& snd) $ vals)
  where
    dec 0 = 6
    dec x = x-1

-- Parsing
parseA = parse
parseB = parse

parse = map (head &&& length) . group . sort . map stoi . words . map (\x -> if x == ',' then ' ' else x)




