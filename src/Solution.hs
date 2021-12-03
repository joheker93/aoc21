{-# LANGUAGE TypeSynonymInstances #-}

module Solution where

import Data.Time (diffUTCTime, getCurrentTime)
import Text.Printf (printf)
import Control.DeepSeq
import Control.Monad (ap)
import qualified Data.Vector as V

data Solution a = Pending | Sol a
type Parser a   = Input -> a
type Input      = String
type Run a b    = a -> Solution b



solveDay :: Run String String -> Run String String -> String -> IO (String,String)
solveDay sa sb p = do
  t1 <- getCurrentTime
  let r1 = sa p
  putStr $ "PART A : " ++ get r1
  t2 <- getCurrentTime
  let d2 = realToFrac $ diffUTCTime t2 t1 :: Double
  putStrLn $ printf (", solved in (%.2f) seconds") d2

  t1 <- getCurrentTime
  let r2 = sb p
  putStr $ "PART B : " ++ get r2
  t2 <- getCurrentTime
  let d2 = realToFrac $ diffUTCTime t2 t1 :: Double
  putStrLn $ printf (", solved in (%.2f) seconds") d2

  let r2 = sb p
  return (get r1,get r2)


instance Show a => Show (Solution a) where
  show Pending = "Pending"
  show (Sol x) = show x


compose :: (Show b, Show c) => (a -> b) -> (a -> c) -> a -> (b,c)
compose f g = ((,) <$> f <*> g)

composeF f g h = f <$> g <*> h

prep :: Show a => a -> Solution String
prep a = Sol (show a)

get  :: Solution String -> String
get Pending = "Pending"
get (Sol a) = a

stoi :: String -> Int
stoi = read :: String -> Int

btoi :: Bool -> Int
btoi True  = 1
btoi False = 0

itob :: Int -> Bool
itob 0 = False
itob _ = True
