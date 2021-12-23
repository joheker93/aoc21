module Days.Day18 where
import Solution
import Data.Char
import Control.Monad

solA :: Run [[(Depth,Int)]] String
solA = prep . snd . head . solve

solB :: Run [[(Depth,Int)]] String
solB = prep . maximum . map snd . concat . map solve . replicateM 2

magnitude :: [(Depth,Int)] -> [(Depth,Int)]
magnitude [x] = [x]
magnitude (x:y:xs) = let ((d1,v1),(d2,v2)) = (x,y)
                     in if d1 == d2 then (d1-1,3*v1+2*v2) : xs else x : magnitude (y:xs)

solve [xs] = until ((==1) . length) magnitude xs
solve (xs:ys:xss) = solve ( run (map (\(d,x) -> (d+1,x)) (xs ++ ys)) : xss)

run xs = if canExplode xs then run (explode xs []) else if canSplit xs then run (split xs) else xs
  where
    canExplode = any ((>4) . fst)
    canSplit   = any ((>=10) . snd)

explode :: [(Depth,Int)] -> [(Depth,Int)] -> [(Depth,Int)]
explode (x:y:xs) b = let ((d1,v1),(d2,v2)) = (x,y)
                     in case compare d1 d2 of
                          LT -> explode (y:xs) (x:b)
                          GT -> explode (y:xs) (x:b)
                          EQ -> if d1 > 4 then reverse (put v1 b) ++ [(d1-1,0)] ++ put v2 xs else explode (y:xs) (x:b)
  where
    put v [] = []
    put v ((d,v'):xs) = (d,v+v') : xs

split :: [(Depth,Int)] -> [(Depth,Int)]
split ((d,x):xs) = if x >= 10 then (d+1,fix floor x) : (d+1, fix ceiling x) : xs
                   else (d,x) : split xs
  where
    fix f = f . (/2) . fromIntegral

--Parsing

type Depth = Int
parseA = map (flip parse 0) . lines
parseB = map (flip parse 0) . lines

parse :: String -> Depth -> [(Depth,Int)]
parse "" _ = []
parse (x:xs) d = case x of
  '[' -> parse xs (d+1)
  ']' -> parse xs (d-1)
  ',' -> parse xs d
  v   -> (d,digitToInt v) : parse xs d
