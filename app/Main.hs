{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Days.Day01 as Day01 (solA,solB,parseA,parseB)
import qualified Days.Day02 as Day02 (solA,solB,parseA,parseB)
import qualified Days.Day03 as Day03 (solA,solB,parseA,parseB)
import qualified Days.Day04 as Day04 (solA,solB,parseA,parseB)
import qualified Days.Day05 as Day05 (solA,solB,parseA,parseB)
import qualified Days.Day06 as Day06 (solA,solB,parseA,parseB)
import qualified Days.Day07 as Day07 (solA,solB,parseA,parseB)
import qualified Days.Day08 as Day08 (solA,solB,parseA,parseB)
import qualified Days.Day09 as Day09 (solA,solB,parseA,parseB)
import qualified Days.Day10 as Day10 (solA,solB,parseA,parseB)
import qualified Days.Day11 as Day11 (solA,solB,parseA,parseB)
import qualified Days.Day12 as Day12 (solA,solB,parseA,parseB)
import qualified Days.Day13 as Day13 (solA,solB,parseA,parseB)
import qualified Days.Day14 as Day14 (solA,solB,parseA,parseB)
import qualified Days.Day15 as Day15 (solA,solB,parseA,parseB)
import qualified Days.Day16 as Day16 (solA,solB,parseA,parseB)
import qualified Days.Day17 as Day17 (solA,solB,parseA,parseB)
import qualified Days.Day18 as Day18 (solA,solB,parseA,parseB)
import qualified Days.Day19 as Day19 (solA,solB,parseA,parseB)
import qualified Days.Day20 as Day20 (solA,solB,parseA,parseB)
import qualified Days.Day21 as Day21 (solA,solB,parseA,parseB)
import qualified Days.Day22 as Day22 (solA,solB,parseA,parseB)
import qualified Days.Day23 as Day23 (solA,solB,parseA,parseB)
import qualified Days.Day24 as Day24 (solA,solB,parseA,parseB)
import qualified Days.Day25 as Day25 (solA,solB,parseA,parseB)

--- Other imports
import Data.Map (Map,lookup)
import qualified Data.Map as Map
import System.Environment (getArgs)
import Text.Parsec
import Text.Printf (printf)
import Data.Time (diffUTCTime, getCurrentTime)
import Solution



data Days = AllDays | Day {day :: Int, input :: String} deriving Show

type Day = Int
type Par a = Parsec String () a

getDay :: Par Days
getDay = do
  string "day"
  day <- (read :: String -> Int) <$> many1 digit
  return Day {day = day, input = "src/input/day" ++ show day ++ ".in"} <|> (eof >> return AllDays)

specInp :: String -> String -> Either ParseError Days
specInp d i = parse getDay "" d >>= \(Day d i') -> return (Day d i)

main :: IO ()
main = do
  day <- getArgs >>= \case
    []       -> return $ Right AllDays
    (x:[])   -> return $ (parse getDay "") x
    (x:y:[]) -> return $ specInp x y

  runSolution day

runSolution :: Either ParseError Days -> IO ()
runSolution (Right (Day d i)) = do
  putStrLn $ "Running day: " ++ show d
  input <- readFile i
  case Map.lookup d env of
    Nothing        -> putStrLn $ "Env broken, Could not find day " ++ show d
    Just (sa , sb) -> solveDay sa sb input >> putStrLn "------"

runSolution (Right AllDays) = do
  putStrLn $ "Running all days"
  let days = convDays AllDays
  mapM_ runSolution days

runSolution (Left x) = error (show x)

convDays :: Days -> [Either ParseError Days]
convDays AllDays = let d = [1..25]
                       i = "src/input/day"
                   in map (\x -> Right (Day x (i ++ show x ++ ".in"))) d
convDays x = [Right x]





env :: Map Int (String -> Solution String, String -> Solution String)
env = Map.fromList . zip [1 ..] $
      [ (Day01.solA . Day01.parseA, Day01.solB . Day01.parseB),
        (Day02.solA . Day02.parseA, Day02.solB . Day02.parseB),
        (Day03.solA . Day03.parseA, Day03.solB . Day03.parseB),
        (Day04.solA . Day04.parseA, Day04.solB . Day04.parseB),
        (Day05.solA . Day05.parseA, Day05.solB . Day05.parseB),
        (Day06.solA . Day06.parseA, Day06.solB . Day06.parseB),
        (Day07.solA . Day07.parseA, Day07.solB . Day07.parseB),
        (Day08.solA . Day08.parseA, Day08.solB . Day08.parseB),
        (Day09.solA . Day09.parseA, Day09.solB . Day09.parseB),
        (Day10.solA . Day10.parseA, Day10.solB . Day10.parseB),
        (Day11.solA . Day11.parseA, Day11.solB . Day11.parseB),
        (Day12.solA . Day12.parseA, Day12.solB . Day12.parseB),
        (Day13.solA . Day13.parseA, Day13.solB . Day13.parseB),
        (Day14.solA . Day14.parseA, Day14.solB . Day14.parseB),
        (Day15.solA . Day15.parseA, Day15.solB . Day15.parseB),
        (Day16.solA . Day16.parseA, Day16.solB . Day16.parseB),
        (Day17.solA . Day17.parseA, Day17.solB . Day17.parseB),
        (Day18.solA . Day18.parseA, Day18.solB . Day18.parseB),
        (Day19.solA . Day19.parseA, Day19.solB . Day19.parseB),
        (Day20.solA . Day20.parseA, Day20.solB . Day20.parseB),
        (Day21.solA . Day21.parseA, Day21.solB . Day21.parseB),
        (Day22.solA . Day22.parseA, Day22.solB . Day22.parseB),
        (Day23.solA . Day23.parseA, Day23.solB . Day23.parseB),
        (Day24.solA . Day24.parseA, Day24.solB . Day24.parseB),
        (Day25.solA . Day25.parseA, Day25.solB . Day25.parseB)
      ]
