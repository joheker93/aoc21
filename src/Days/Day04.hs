{-# LANGUAGE LambdaCase #-}
module Days.Day04 where
import Solution
import Control.Arrow ((&&&))
--import Control.Applicative (liftA2)
import Data.List (transpose,init,elemIndex,maximum)

--Part 1
solA :: Run ([Int],[[[Int]]]) String
solA = prep  . complete run isWinner
  where dig  = map . map . map

complete f g = calc . (f g <$> fst <*> dig ((,) <$> const False <*> id) . snd)

run f [] _ = (-1,[])
run f (x:xs) cs = case go (x:xs) cs [] of
  [sol]  -> (x,sol)
  rep    -> run f xs rep
  where
    go _ [] s          = s
    go (x:xs) (c:cs) s = let m = mark x c
                         in if (f m) then [m] else go (x:xs) cs (s ++ [m])

--- Part 2
solB :: Run ([Int],[[[Int]]]) String
solB = prep . calc . (flip (untilWin isWinner) <$>
         ((flip (!!) <$> maybe undefined id . ((elemIndex <$> maximum <*> id) . (go isWinner <$> fst <*> inp)) <*> inp)) <*> fst)
  where
    inp = dig ((,) <$> const False <*> id) . snd
    go f xs = map (counts f xs)

--Utils
dig = map . map . map

untilWin f _ [] = (-1,[])
untilWin f (x:xs) c = let m = mark x c
                      in if (f m) then (x,m) else untilWin f xs m

counts f [] _     = 0
counts f (x:xs) c = let m = mark x c
                    in if (f m) then 0 else 1 + counts f xs m

calc :: (Int,[[(Bool,Int)]]) -> Int
calc = (*) <$> fst <*> foldr (\(b,x) a -> if not b then x + a else a) 0 . concat . snd

mark :: Int -> [[(Bool,Int)]] -> [[(Bool,Int)]]
mark x = map (map (\(b,v) -> (if v == x then True else b,v)))

isWinner :: [[(Bool,Int)]] -> Bool
isWinner = let chart = map (map fst) . ((++) <$> id <*>  transpose)
           in or . map and . chart

-- Parsing
parseA = parse
parseB = parse

parse :: Parser ([Int], [[[Int]]])
parse = ((map stoi . words . map rep . head) &&& (init . p)) . lines
  where
    rep ',' = ' '
    rep x   = x
    p = \case
      [] -> []
      xs -> (:) <$> ((map (map  stoi) . map words) . take 5 . drop 1) <*> (p . drop 5) $ (drop 1 xs)
