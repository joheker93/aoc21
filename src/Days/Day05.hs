module Days.Day05 where
import Solution
import Control.Arrow((&&&),(***))
import Control.Monad(join)
import Data.List (sort,group)

solA :: Run [Line] String
solA = prep . run straights

solB :: Run [Line] String
solB = prep . run diags

run f = length . filter ((>=2) . length) . group . sort . concat . map f

straights :: Line -> [Point]
straights ((x1,y1),(x2,y2))
  | x1 == x2  = [(x1,v) | v <- [min y1 y2 .. max y1 y2]]
  | y1 == y2  = [(v,y1) | v <- [min x1 x2 .. max x1 x2]]
  | otherwise = []

diags :: Line -> [Point]
diags t@((x1,y1),(x2,y2)) = if ss == [] then go x1 x2 y1 y2 else ss
  where
    go x g y g2
      | x == g && y == g2 = [(x,y)]
      | otherwise         = (x,y) : go (dir x g) g (dir y g2) g2
    dir v g = if v > g then v-1 else v+1
    ss = straights t

--Parsing
parseA = parse
parseB = parse

type Point = (Int,Int)
type Line = (Point,Point)

parse :: Parser [Line]
parse = map (join (***) (tol . map stoi . words . map rep)) . map p . lines
  where
    p       = (takeWhile (/= ' ')) &&& (drop 2 . dropWhile (/= '>'))
    rep ',' = ' '
    rep x   = x
    tol     = (!! 0) &&& (!! 1)
