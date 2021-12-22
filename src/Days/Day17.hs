module Days.Day17 where
import Solution

--Bruteforce ¯\_(ツ)_/¯

solA :: Run String String
solA = prep . maximum . map ((,) <$> maxHeight 0 0 0 <*> id) . const (run [] tests)
  where
    tests = [(x,y) | x <- [0..156], y <- [0..103]]

solB :: Run String String
solB = prep . length . const (run [] tests)
  where
    tests =  [(x,y) | x <- [0..156], y <- [-103..103]]

run env [] = env
run env ((xv,yv):tests) = case probe xv yv 0 0 xv yv of
  Nothing -> run env tests     -- Miss
  Just p  -> run (p:env) tests -- Hit

probe xv yv x y xvo yvo =
  let (x',y')   = (x+xv,y+yv)
      (xv',yv') = (drag xv, yv-1)
      inSide    = inTarget x' y'
      missed    = x' > x2 ||  y' < y1
  in if inSide then Just (xvo,yvo) else if missed then Nothing else  (probe xv' yv' x' y' xvo yvo)

drag v = if v > 0 then v-1 else if v < 0 then v+1 else v

inTarget x y = (x,y) `elem` target x1 x2 y1 y2

maxHeight m x y (xv,yv) | y < m = m
                        | otherwise =
                          let (x',y')   = (x+xv,y+yv)
                              (xv',yv') = (drag xv,yv-1)
                          in maxHeight y x' y' (xv',yv')

target x1 x2 y1 y2 = [(x,y) | x <- [x1..x2],y <- [y1..y2]]

-- Parsing* Just hardcoded input values
parseA = id
parseB = id

--Input values
x1 = 135
x2 = 155
y1 = -102
y2 = -78
