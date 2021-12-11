module Days.Day08 where
import Solution

import Data.List (sum, (\\),sort,sortBy)
import Control.Arrow ((&&&))
import Data.Ord(comparing)

solA :: Run [[String]] String
solA = prep . sum . map go
  where go = length . filter (flip elem [2,4,3,7] . length)

solB :: Run ([[String]],[[String]]) String
solB (snds,xs) = let s'  = map (map sort) snds
                     z   = zip s' (map solve xs)
                     fix = uncurry (flip (map . flip lookup))
                     digs :: [Int] -> Int
                     digs = foldl (\n d -> 10*n+d) 0
                 in (prep . sum . map digs . map (maybe undefined id . sequence)) (map fix z)

solve :: [String] -> [(String,Int)]
solve (one:seven:four:xs) =
  let
        (s1:s2:s3:_) = filter ((==6) . length ) xs
        (six,r1,r2)  = if length (s1 \\ one)  == 5 then (s1,s2,s3) else if length (s2 \\ one) == 5 then (s2,s1,s3) else (s3,s1,s2)
        (zero,nine)  = if length (r1 \\ four) == 2 then (r2,r1)    else (r1,r2)

        two   = (eight \\ f) \\ b
        three = nine   \\ b
        five  = nine   \\ c
        eight = last xs

        b   = (bdg  \\ d) \\ g
        c   = eight \\ six
        d   = eight \\ zero
        bdg = five  \\ seven
        f   = one   \\ c
        g   = (nine \\ seven) \\ four

        l = [zero, one, two, three, four, five, six, seven, eight, nine]
  in zip l [0..]

-- Parsing
parseA :: Parser [[String]]
parseA = fst . parse

parseB :: Parser ([[String]],[[String]])
parseB = parse

parse :: String -> ([[String]],[[String]])
parse = (map snd &&& map (sortBy (comparing length)) . map (map sort) . map fst) . map p . lines

p :: String -> ([String],[String])
p = words . takeWhile (/= '|') &&& words . drop 2 . dropWhile (/='|')
