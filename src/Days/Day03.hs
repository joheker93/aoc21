module Days.Day03 where
import Solution
import Data.List(intersperse,transpose)
import Data.Bits
import Control.Monad (join)
import Control.Arrow ((***))

solA :: Run [[Int]] String
solA = prep . uncurry (*) . join (***) bintodec . compose id (map not) . reverse . run (uncurry (>)) map

solB :: Run [[Int]] String
solB = prep . uncurry (*) . join (***) (bintodec . reverse . map itob) . compose (go 0 (>=)) (go 0 (<))
  where go b f l    = if (composeF f fst snd ((run id map l) !! b)) then rep 1 b l f else rep 0 b l f
        rep v b l f = let filt = (filter ((== v) . (!! b)) l)
                      in case filt of
                           [x] -> x
                           xs  -> go (b+1) f filt

run f g = map (f . foldl (\(a,b) x -> if x == 1 then (a+1,b) else (a,b+1)) (0,0)) . transpose

bintodec :: [Bool] -> Int
bintodec = foldr (\x y -> fromEnum x + 2*y) 0

--Parsing
parseA = parse
parseB = parse

parse :: Parser [[Int]]
parse = map (map stoi . words . intersperse ' ') . lines





