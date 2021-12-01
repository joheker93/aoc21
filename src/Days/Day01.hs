module Days.Day01 where
import Solution


solA :: Run [Int] String
solA xs = prep $ head $ [x*y | x <- xs , y <- xs, x+y == 2020] >>= return

solB :: Run [Int] String
solB xs = prep $ head [x*y*z | x <- xs, y <- xs, z <- xs, x+y+z == 2020]


parseA :: Parser [Int]
parseA = parse

parseB :: Parser [Int]
parseB = parse

parse :: Parser [Int]
parse = map (read :: String -> Int) . words

