module Days.Day15 where
import Solution hiding (neighs)


import qualified Data.Map as M
import Data.List (intersperse)
import qualified Data.Graph.Inductive.Query.SP as SP
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Tree as T
import Data.Char(digitToInt)

-- Running through an Shortest path library, not super fast

solA :: Run [[Int]] String
solA xs = let m = buildM M.empty 0 xs
              gr = G.mkGraph (nodes m) (edges m (indexes 100 100) 100) :: T.Gr Int Int
              spL = SP.spLength 0 9999 gr
          in prep $ SP.spLength 0 9999 gr



solB :: Run [[Int]] String
solB xs = let m = buildM M.empty 0 (repl2 (repl xs))
              gr = G.mkGraph (nodes m) (edges m (indexes 500 500) 500) :: T.Gr Int Int
              sp = SP.sp 0  249999  gr
          in prep $ SP.spLength 0 249999 gr
  where
    repl2 xs =  concat [xs, map (fmod 1) xs, map (fmod 2) xs, map (fmod 3) xs, map (fmod 4) xs]

    repl [] = []
    repl (xs:xss)  = concat [xs,fmod 1 xs, fmod 2 xs, fmod 3 xs, fmod 4 xs] : repl xss
    fmod x = map ((\x -> if x > 9 then mod x 10 + 1 else x) . (+x))

edges :: M.Map Int Int -> [(Int,Int)] -> Int -> [G.LEdge Int]
edges m [] _     = []
edges m (x:xs) s = edgeN x m s ++ edges m xs s

edgeN (x,y) m s = let ids = [b * s + a | (a,b) <- [(x,y-1),(x+1,y),(x,y+1),(x-1,y)], a >= 0 && a < s, b >= 0 && b < s]
                      id = y*s+x
                      edges = foldl (\a n -> case M.lookup n m of
                                        Nothing -> a
                                        Just v  -> (id,n,v) : a) [] ids
                  in edges

nodes :: M.Map Int Int -> [G.LNode Int]
nodes = map (\x -> (x,x)) . map fst . M.toList

--Parsing
parseA = parse
parseB = parse

parse = map (map stoi . words . intersperse ' ') . lines

buildM :: M.Map Int Int -> Int -> [[Int]] -> M.Map Int Int
buildM m i [] = m
buildM m i (xs:xss) = let (m',i') = build m xs i
                      in buildM m' i' xss

build :: M.Map Int Int -> [Int] -> Int -> (M.Map Int Int,Int)
build m [] i     = (m,i)
build m (x:xs) i = build (M.insert i x m) xs (i+1)

indexes h w = [(x,y) | x <- [0..w-1],y <- [0..h-1]]
