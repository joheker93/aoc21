module Days.Day12 where
import Solution
import Control.Arrow ((&&&))
import Data.List (sortBy,nub,sum)
import Data.Char(isUpper)
import qualified Data.Map as M

solA :: Run (M.Map String [String]) String
solA = prep  . paths "start" [] 0 "" (const3 False,const3 False)

-- Tons of unnecessary pathing going on here
solB :: Run ([String], (M.Map String [String])) String
solB (atoms,l) = let tot     = map (\x -> paths "start" [] 0 x (c1,c2) l) atoms
                     isu     = not . all isUpper
                     c1      = \n a c -> n == a && c>=2 && isu n
                     c2      = \n a c -> n == a && isu n
                     commons = paths "start" [] 0 "" (const3 False, const3 False) l
                  in prep $ sum tot - (length atoms - 1)*commons

type F a = String -> a -> Int -> Bool
paths :: String -> [String] -> Int -> a -> (F a,F a) -> M.Map String [String] -> Int
paths "end" vis _ _ _ _= 1
paths n vis c a (f,g) ns | f n a c      = 0
                         | g n a c      = update (c+1)
                         | n `elem` vis = 0
                         | otherwise    = update c
  where
    update c = case M.lookup n ns of
      Nothing -> 0
      Just xs -> sum $ map (\x -> paths x (if all isUpper n then vis else (n:vis)) c a (f,g) ns) xs

const3 a b c d = a

-- Parsing
parseA = parse
parseB = atoms &&& parse

atoms = filter (\x -> x /= "end" && x /= "start" && not (all isUpper x)) . nub . concatMap ((:) <$> fst <*> (:[]) . snd) . map split . lines

parse = (toMap <$> revL <*> flip toMap M.empty)  . map split . lines
  where
    revL = map (snd &&& fst)

split = takeWhile (/='-') &&& drop 1 . dropWhile (/='-')

toMap :: [(String,String)] -> M.Map String [String] -> M.Map String [String]
toMap [] m = m
toMap ((k,v):ns) m = case M.lookup k m of
  Nothing -> toMap ns (M.insert k [v] m)
  Just xs -> toMap ns (M.insert k (v:xs) m)
