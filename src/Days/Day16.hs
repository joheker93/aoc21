{-# LANGUAGE LambdaCase #-}
module Days.Day16 where
import Solution hiding (Parser)
import Numeric (readHex)

import Data.Char (digitToInt)
import Data.List (foldl',sum)
import Text.Parsec
import Text.Parsec.String
import Data.Maybe (listToMaybe)
import Data.Maybe (mapMaybe)
import Text.Printf

data Packet = Literal Version Type String | Operator Version Type [Packet] | End deriving Show
type Version = String
type Type    = String

solA :: Run String String
solA = prep . countVs . parse pPacket "" . pad
  where
    countVs = \case
      Left x   -> error $ "RIP"
      Right ps -> foldr (\x a -> toDec x + a) 0 $ flat ps
    flat (Literal v _ _ )  = [v]
    flat (Operator v _ ps) = v : foldl (\a x -> flat x ++ a) [] ps


solB :: Run String String
solB =  prep  . solve . parse pPacket "" . pad
  where solve = \case
          Left e -> error $ "Rip"
          Right p -> packetVal p

packetVal :: Packet -> Int
packetVal (Literal _ _ v) = toDec v
packetVal (Operator _ t ps) = case toDec t of
  0 -> sum $ map packetVal ps
  1 -> product $ map packetVal ps
  2 -> minimum $ map packetVal ps
  3 -> maximum $ map packetVal ps
  5 -> compeq (>)  ps
  6 -> compeq (<)  ps
  7 -> compeq (==) ps
  where
    compeq f ps = let [l,r] = ps in if f (packetVal l) (packetVal r) then 1 else 0

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- Parsing
parseA = parse'
parseB = parse'

parse' = concat . mapMaybe htob

pad s | length s `mod` 4 == 0 = s
      | otherwise = pad ('0':s)

htob :: Char -> Maybe String
htob c = case readHex [c] of
    (x,_):_ -> Just $ printf "%04b" (x::Int)
    _       -> Nothing

values :: Parser String
values = digit >>= \case
  '0' -> count 4 digit
  '1' -> count 4 digit >>= \ds -> values >>= return . ((++) <$> const ds <*> id)

pPacket :: Parser Packet
pPacket = do
  (v,t) <- (,) <$> count 3 digit <*> count 3 digit
  case t of
    "100" -> values >>= return . Literal v t
    x     -> digit >>= \case
      '0' -> toDec <$> count 15 digit >>= iter >>= return . Operator v t
      '1' -> toDec <$> count 11 digit >>= flip count pPacket >>= return . Operator v t

  where
    iter :: Int -> Parser [Packet]
    iter l = do
      g <- length <$> getInput
      p <- pPacket
      g2 <- length <$> getInput
      if (g - g2) < l then do
        ps <- iter (l - (g - g2))
        return $ p : ps else do
        return $ [p]

