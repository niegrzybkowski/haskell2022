module Kalkulator where

parseOp :: (Num a) => [Char] -> a -> a -> a
parseOp op 
    | op == "+" = (+)
    | op == "*" = (*)
    | otherwise = error "Operator not + or *"

main :: IO ()
main = calc

calc :: IO ()
calc = do
    opString <- getLine;
    num1 <- readLn :: IO Int;
    num2 <- readLn :: IO Int;
    print (parseOp opString num1 num2);

calc' :: IO ()
calc' = (parseOp <$> getLine <*> (readLn :: IO Int) <*> (readLn :: IO Int)) >>= print