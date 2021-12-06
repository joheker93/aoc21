{-# LANGUAGE LambdaCase #-}

module Days.Day04 where
import Solution
import Control.Arrow ((&&&))
import Data.List (elemIndex,transpose)

type Bingo = ([Int], [Chart])
type Chart = [[Int]]

solA :: Run Bingo String
solA = complete minimum

solB :: Run Bingo String
solB = complete maximum

-- Main solver
complete f = prep . calc . (flip (untilWin isWinner) <$>
         ((flip (!!) <$> maybe undefined id . ((elemIndex <$> f <*> id) . (go isWinner <$> fst <*> inp)) <*> inp)) <*> fst)
  where
    inp     = dig ((,) <$> const False <*> id) . snd
    go f xs = map (counts f xs)
    dig     = map . map . map

--Utils
untilWin f _ []     = (-1,[])
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
