module Days.Day13 where
import Solution
import Control.Arrow ((&&&))
import Data.List (transpose,nub)

solA :: Run ([(Int,Int)],[(Char,Int)]) String
solA = prep . length . nub .  (run <$> (:[]) . head . snd <*> fst)

solB :: Run ([(Int,Int)],[(Char,Int)]) String
solB = prep . unlines . canvas . (run <$> snd <*> fst)

run [] ps = ps
run (f:fs) ps = run fs (map (folder f) ps)
  where
    folder ('y',v) (x,y) = (x,if y < v then y else 2*v-y)
    folder ('x',v) (x,y) = (if x < v then x else 2*v-x,y)

replace (x,y) v = rep x (rep y (const v))
  where rep v f xs = [if i == v then f x else x | (x,i) <- zip xs [0..]]

canvas :: [(Int,Int)] -> [[Char]]
canvas ps = let xs = maximum $ map fst ps
                ys = maximum $ map snd ps
            in transpose $ paint ps xs ys
  where
    paint ps xs ys = let c = replicate (xs+1) (replicate (ys+1) '.')
                     in foldl (\a p -> replace p '#' a) c ps


--Parsing
parseA = parse
parseB = parse

parse = (map points . takeWhile (/=[]) &&& map folds . drop 1 . dropWhile (/=[])) . lines
  where
    points = (\[a,b] -> (stoi a,stoi b)) . words . map rep
    folds  = f . last . words
    f (x:_:val) = (x,stoi val)
    rep ',' = ' '
    rep x   = x
