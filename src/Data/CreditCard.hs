{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.CreditCard
    ( classifyCard
    , CreditCard(..)
    , CreditCardClassification(..)
    , CardIssuer(..)
    , ClassificationFailure(..)
    ) where

import Data.Char
import GHC.Generics

data CardIssuer
  = AmericanExpress
  | Visa
  | MasterCard
  | Maestro
  | Discover
  | Unknown
  deriving (Show)

data ClassificationFailure
  = TooShort
  | InvalidCharacter Char
  deriving (Show)

data CreditCardClassification = CreditCardClassification
  { creditCardClassificationIssuer :: CardIssuer
  } deriving (Show)

newtype CreditCard = CreditCard String

classifyCard :: CreditCard -> Either ClassificationFailure CreditCardClassification
classifyCard (CreditCard str) = go 0 (filter (\c -> not (isPunctuation c) && not (isSpace c)) str) (CreditCardClassification Unknown)
  where
    go 0 (c:cs) st =
      let next = go (1 :: Int) cs
          skip = next st
      in case c of
      '0' -> skip
      '1' -> skip
      '2' -> case cs of
        (c1:c2:c3:cs') -> let x = read [c1, c2, c3] in if x >= 221 && x <= 720
          then go 3 cs' (st { creditCardClassificationIssuer = MasterCard })
          else skip
        _ -> skip
      '3' -> case cs of
        (c':cs') -> case c' of
          '4' -> go 2 cs' (st { creditCardClassificationIssuer = AmericanExpress })
          '7' -> go 2 cs' (st { creditCardClassificationIssuer = AmericanExpress })
          _   -> skip
        [] -> Left TooShort
      '4' -> next (st { creditCardClassificationIssuer = Visa })
      '5' -> case cs of
        (c':cs') ->
          if | c' == '0' || c' >= '6' && c' <= '8' -> go 2 cs' $ st { creditCardClassificationIssuer = Maestro }
             | c' >= '1' && c' <= '5'              -> go 2 cs' $ st { creditCardClassificationIssuer = MasterCard }
             | otherwise -> skip
        [] -> skip
      '6' -> skip
      '7' -> skip
      '8' -> skip
      '9' -> skip
      _   -> Left $ InvalidCharacter c
    go x (c:cs) st = go (x + 1) cs st
    go x [] st = if x >= 6 then Right st else Left TooShort

-- Sample cards
{-
American Express
378282246310005

American Express
371449635398431

American Express Corporate
378734493671000

Discover
6011111111111117

Discover
6011000990139424

MasterCard
5555555555554444

MasterCard
5105105105105100

Visa
4111111111111111

Visa
4012888888881881

Visa
4222222222222
Note : Even though this number has a different character count than the other test numbers, it is the correct and functional number.
-}
