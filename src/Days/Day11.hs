module Days.Day11 where
import Solution
import Data.List (intersperse)

solA :: Run [[Int]] String
solA = prep . fst . \m -> run (100,0,0,m) (\(n,_,_,_) -> n == 0)

solB :: Run [[Int]] String
solB = prep . snd . \m -> run (0,0,0,m) (\(_,_,_,m) -> nils m)
  where nils =  all (==0) . concat

run t@(n,v,c,m) f | f t = (v,c)
                  | otherwise = let t         = tick m
                                    (sx,sy)   = (length $ head m, length m)
                                    (val,res) = cont t (indexes sx sy) sx sy []
                                in run ((n-1),val+v,c+1,res) f

cont m ps sx sy i = let (flashed,res) = go m ps sx sy [] in case flashed of
  [] -> let i' = concat i in (length i', reset i' m)
  xs -> cont res ps sx sy (flashed : i)
 where
   go m [] _ _ i = (i,m)
   go m (p:ps) sx sy i = let (i',res) = flash p i sx sy m
                         in go res ps sx sy i'


reset []     m = m
reset (i:is) m = reset is (replace i 0 m)

flash :: (Int,Int) -> [(Int,Int)] -> Int -> Int -> [[Int]] -> ([(Int,Int)],[[Int]])
flash t@(x,y) i sx sy m | inBound t = if ny t > 9  && not (elem t i) then (t:i,flashes) else (i,m)
                        | otherwise = (i,m)
  where
    inBound (x,y) = x >= 0 && x < sx && y >= 0 && y < sy
    ny (x,y)      = m !! x !! y
    flashes       = foldl (\a t' -> if inBound t' then replace t' (ny t' + 1) a else a) (replace t 0 m) neighs
    neighs        = [(x',y') | x' <- [x-1..x+1], y' <- [y-1..y+1], (x',y') /= (x,y)]

replace (x,y) v = rep x (rep y (const v))
  where rep v f xs = [if i == v then f x else x | (x,i) <- zip xs [0..]]

tick :: Num a => [[a]] -> [[a]]
tick = foldl (\a x -> map (+1) x : a) []

indexes h w = [(x,y) | x <- [0..w-1],y <- [0..h-1]]

--Parsing
parseA = parse
parseB = parse

parse = map (map stoi . words . intersperse ' ') . lines
