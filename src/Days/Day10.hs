module Days.Day10 where
import Solution

import Data.List (intersperse,sort,foldl')

solA :: Run [[String]] String
solA = prep . sum . concat . map (col . snd . build [] [] [])
  where
    col = foldr (\x a -> case x of
                Expected c1 c2 -> maybe undefined id (lookup c2 scores) : a
                x              -> a) []

solB :: Run [[String]] String
solB = prep . ((!!) <$> id <*> (div <$> length <*> const 2)) . sort . filter (/=0) .  map (res . col . fst . build [] [] [])
  where
    res = foldl (\a x -> a*5+x) 0
    col = foldr (\x a -> maybe undefined id (lookup x scores2) : a) []


build ::  [String] -> [Char] -> [Error Char] -> [String] -> (String, [Error Char])
build c e [] []    = let r = concat (reverse c) in (e,if allMc r then [] else [Incomplete])
build c e f []     = ([],f)
build c e f (x:xs) = case x of
  "[" -> form ']'
  "(" -> form ')'
  "{" -> form '}'
  "<" -> form '>'
  [x] -> reform x
  where
    form i = build (x:c) (i:e) f xs
    reform i | e == []   = build (x:c) e (SurpriseClose i : f) xs
             | otherwise = if head e == i then build (x:c) (tail e) f xs else build  (x:c) (tail e) (err (head e) i : f) xs

    err = Expected

allMc :: String -> Bool
allMc xs = mc ('[',']') 0 xs &&
           mc ('(',')') 0 xs &&
           mc ('{','}') 0 xs &&
           mc ('<','>') 0 xs
  where
    mc :: (Char,Char) -> Int -> String -> Bool
    mc (o,c) i [] = i == 0
    mc t@(o,c) i (x:xs) | x == c = mc t (i-1) xs
                        | x == o = mc t (i+1) xs
                        | otherwise = mc t i xs

scores :: [(Char,Int)]
scores = [(')',3),(']',57),('}',1197),('>',25137)]

scores2 :: [(Char,Int)]
scores2 = [(')',1),(']',2),('}',3),('>',4)]

data Error c = Expected c c | Incomplete | SurpriseClose Char

-- Parsing
parseA = parse
parseB = parse

parse :: String -> [[String]]
parse = map (words . intersperse ' ') . lines
