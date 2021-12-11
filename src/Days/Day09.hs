{-# LANGUAGE FlexibleContexts #-}
module Days.Day09 where
import Solution

import Data.List (intersperse,nub,sortBy)
import Data.Ord (comparing)
import Control.Arrow ((&&&))

solA :: Run Mat String
solA  = prep . sum . map snd . map run . neighs

run xs@(x:_) = if all (\p -> gx p < gy p) xs then (fst x , snd (fst x) + 1) else (fst x, 0)
  where
    gy (_,b) = snd b
    gx (a,_) = snd a

solB :: Run Mat String
solB xs = let lpts      = map (fst . fst) . filter ((/=0) . snd) $ map run $ neighs xs
              (ogi,ogv) = map (map fst) &&& map (map snd) $ xs
              (sx,sy)   = length &&& length . head $ xs
              basins    = map nub $ map (\(x,y) -> let v = ogv !! x !! y in exps sx sy ogv (x,y) (v,v) []) lpts
              [f,s,t]   = take 3 $ sortBy (flip (comparing length)) basins
          in prep (length f * length s * length t)

exps :: Int -> Int -> [[Int]] -> (Int,Int) -> (Int,Int) ->  [(Int,Int)] -> [(Int,Int)]
exps sx sy m t@(x,y) (v1,v2) a | prop = iter t
                               | otherwise = a
  where
    prop    = inBound && not (elem t a) && incr && nv /= 9
    incr    = nv >= v1 || nv == v2
    iter v  = concat [exps sx sy m t (nv+1,nv) (v:a) | t <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]]
    nv      = (m !! x) !! y
    inBound = x >= 0 && x < sx && y >= 0 && y < sy

--Parsing
type Mat = [[((Int,Int),Int)]]
parseA = parse
parseB = parse

parse :: String -> Mat
parse xs = let m = p xs
               l = length $ head m
               cs = chunks l (inds l)
           in zipWith zip cs m
  where
    chunks _ [] = []
    chunks n xs = take n xs : chunks n (drop n xs)
    p = map (map stoi . words . intersperse ' ') . lines
    inds n = [(a,b) | a <- [0..n-1], b <- [0..n-1]]
