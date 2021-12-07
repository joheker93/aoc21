module Days.Day07 where
import Solution
import Data.List(sort)

solA :: Run [Int] String
solA =  prep . sum . (map <$> dist . med <*> id)
  where
    med = (!!) <$> sort <*> flip div 2 . length

solB :: Run [Int] String
solB = prep . minimum . map sum . (flip dynDist <$> maximum <*> id)
  where
    dynDist _ 0  = []
    dynDist xs x = map (dSum x) xs : dynDist xs (x-1)
    dSum  = ((\k -> k*(k+1) `div` 2) .) . dist

dist a b = abs (a-b)

-- Parsing
parseA = parse
parseB = parse

parse = map stoi . words . map rep
  where
    rep ',' = ' '
    rep x   = x
