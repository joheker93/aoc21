module Days.Day14 where
import Solution
import Control.Arrow ((&&&))
import qualified Data.Map as M

type Env   = M.Map (Char,Char) Char
type Count = M.Map Char Int
type Pairs = M.Map (Char,Char) Int

solA :: Run ([(Char,Char)],Env) String
solA = prep . solve 10

solB :: Run ([(Char,Char)],Env) String
solB = prep . solve 40

solve :: Int -> ([(Char,Char)],Env) -> Int
solve x = calc . (run <$> const x <*> buildMap . fst <*> snd <*> buildMap . pretty . fst)
  where
    calc = ((-) <$> maximum <*> minimum) . map snd . M.toList . snd
    run 0 ps _ c = (ps,c)
    run n ps e c = let (i,c') = insert (M.toList ps) e c
                   in run (n-1) i e c'

    buildMap :: Ord a =>  [a] -> M.Map a Int
    buildMap = foldl (\a x -> M.insertWith (+) x 1 a) M.empty

insert :: [((Char,Char),Int)] -> Env -> Count -> (Pairs,Count)
insert [] _ c = (M.empty,c)
insert ((t@(a,b),x):xs) e c = let v  = maybe undefined id (M.lookup t e)
                                  c' = M.insertWith (+) v x c
                                  (res,counts) = insert xs e c'
                              in (build (((a,v),x) : ((v,b),x): M.toList res),counts)
  where
    build = foldl (\a (p,x) -> M.insertWith (+) p x a) M.empty

pretty :: [(Char,Char)] -> String
pretty [(x,y)] = [x,y]
pretty ((x,_):xs) = x : pretty xs

parseA = parse
parseB = parse

parse = ((zip <*> tail) . head &&& M.fromList . map table . drop 2) . lines
  where
    table = ((\[a,b] -> (a,b)) . take 2) &&& last
