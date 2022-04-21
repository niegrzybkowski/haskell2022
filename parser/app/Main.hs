{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Data.Char

data Parser a = Parser { parse :: String -> Maybe (a, String) }

run :: Parser a -> String -> Maybe a
run (Parser p) input = fmap fst (p input)

runEnsureConsumeAll :: Parser a -> String -> Maybe a
runEnsureConsumeAll (Parser p) input = case p input of
  Just (v, "") -> Just v
  _ -> Nothing

readChar :: Parser Char
readChar = Parser fun
  where
    fun [] = Nothing
    fun (c:cs) = Just (c, cs)

failure :: Parser a
failure = Parser $ const Nothing

instance Functor Parser where
  fmap f (Parser pf) = Parser $ (fmap (\(x,s) -> (f x, s))) .  pf

instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  (Parser f1) <*> (Parser f2) =
    Parser $ \s -> do
        (fun, s1) <- f1 s;
        (arg, s2) <- f2 s1;
        return  (fun arg, s2)

instance Monad Parser where
  Parser f >>= fun = Parser $ \s -> do
    (arg, s1) <- f s;
    (parse $ fun arg) s1

--  fail _ = failure

instance Alternative Parser where
  empty = failure
  p1 <|> p2 = Parser $ \s -> (parse p1 s <|> parse p2 s)

ensureChar :: Char -> Parser Char
ensureChar c = readChar >>= (\d -> if c == d then return d else failure)

ensureMatching :: String -> Parser Char
ensureMatching set = readChar >>= (\d -> if d `elem` set  then return d else failure)

readMatching :: String -> Parser String
readMatching set = some $ ensureMatching set

eatChars :: String -> Parser ()
eatChars set = void $ many $ ensureMatching set

parseInt :: String -> Integer
parseInt l = parseRec 0 l
  where
    parseRec :: Integer -> String -> Integer
    parseRec sum (x:xs) = (parseRec $! ((toInteger $ digitToInt x)+10*sum)) xs
    parseRec sum [] = sum

readInt :: Parser Integer
readInt = parseInt <$> readMatching "0123456789"

readFloat :: Parser Double
readFloat = do
  gt1 <- readInt;
  _ <- ensureChar '.';
  lt1 <- readInt;
  pure $ fromIntegral gt1 + toDecimalPart lt1
  where
    toDecimalPart :: Integer -> Double
    toDecimalPart n
      | n == 0 = 0.0
      | n >  0 = let n' = fromIntegral n in
        n' / (10 ** fromInteger (floor (1.0 + logBase 10.0 n')))
      | otherwise = error "dd"

readListSubparser :: Parser a -> Parser [a]
readListSubparser subparser = do
  n <- subparser;
  rest <- (ensureChar ',' >> readListSubparser subparser) <|> (return [])
  return (n:rest)

readIntList :: Parser [Integer]
readIntList = readListSubparser readInt

readFloatList :: Parser [Double]
readFloatList = readListSubparser readFloat

readCSV :: Parser [[Double]]
readCSV = do
  firstLine <- readFloatList;
  otherLines <- (ensureChar '\n' >> readCSV) <|> return []
  return $ firstLine:otherLines

-- readDF :: Parser [[Double]]
-- readDF = do
--   csv <- readCSV

data IntExpr =
  MultExpr IntExpr IntExpr
  | MinusExpr IntExpr IntExpr
  | PlusExpr IntExpr IntExpr
  | PowExpr IntExpr IntExpr
  | IntLeaf Integer
  deriving (Show)

eval :: IntExpr -> Integer
eval (IntLeaf x) = x
eval (PlusExpr x y) = (eval x) + (eval y)
eval (MinusExpr x y) = (eval x) - (eval y)
eval (MultExpr x y) = (eval x) * (eval y)
eval (PowExpr x y) = myPow (eval x) (eval y)

-- nie obsługuję liczb ujemnych
myPow :: Integer -> Integer -> Integer
myPow x y = myPowRec 1 y
  where
    myPowRec prod y
      | y == 0 = prod
      | y > 0 = (myPowRec $! (prod*x)) (y-1)
      | otherwise = error "Negative powers are not supported"


stringToOp :: Char -> (IntExpr -> IntExpr -> IntExpr)
stringToOp '-' = MinusExpr
stringToOp '+' = PlusExpr
stringToOp '*' = MultExpr
stringToOp '^' = PowExpr
stringToOp _ = error "Unsupported Expression"

parseOp :: String -> Parser (IntExpr -> IntExpr -> IntExpr)
parseOp ops = stringToOp <$> ensureMatching ops

parsePrio3R :: IntExpr -> Parser IntExpr
parsePrio3R lhs = ((parseOp "^") <*> pure lhs <*> (parseVal >>= parsePrio3R)) <|> return lhs

parsePrio1 :: IntExpr -> Parser IntExpr
parsePrio1 lhs =
  ((parseOp "+-" <*> pure lhs <*> (parseVal >>= parsePrio3R >>= parsePrio2)) >>= parsePrio1) <|> return lhs

parsePrio2 :: IntExpr -> Parser IntExpr
parsePrio2 lhs =
  ((parseOp "*" <*> pure lhs <*> (parseVal >>= parsePrio3R)) >>= parsePrio2) <|> return lhs 

parseExpr :: Parser IntExpr
parseExpr = parseVal >>= parsePrio3R >>= parsePrio2 >>= parsePrio1

parseParen :: Parser IntExpr
parseParen = ensureChar '(' *> parseExpr <* ensureChar ')'

parseVal :: Parser IntExpr
parseVal = parseParen <|> IntLeaf <$> readInt

main :: IO ()
main = print ""
