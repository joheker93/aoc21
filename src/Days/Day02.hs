module Days.Day02 where
import Solution

solA :: Run [(String,Int)] String
solA = run (fst . fst) (snd . fst)

solB :: Run [(String,Int)] String
solB = run (fst . fst) snd

run f g = prep . composeF (*) f g . foldl (flip update) ((0,0),0)

type Env a b = (a,b) -> (a,b)

update :: (String,Int) -> Env  (Int,Int) Int
update ("forward",f) = compose (compose ((+f) . fst . fst) (snd . fst)) (composeF (+) ((*f) . snd . fst) snd)
update ("up",u)      = compose (compose (fst . fst) (flip (-) u . snd . fst)) snd
update ("down",d)    = compose (compose (fst . fst) ((+d) . snd . fst)) snd

--parsing
parseA = parse
parseB = parse

parse = map (toT . words) . lines
  where toT [x,y] = (x,stoi y)
