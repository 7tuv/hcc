module Tokenizer (
        tokenizer
    ) where

import Data.Char (isDigit, isLower)
import DataType

---- トークナイザ ----
tokenizer :: String -> [Token]
tokenizer ""        = []
tokenizer (x:xs)
    | x == ' '  = tokenizer xs
    | x == '+'  = Symbol "+" : tokenizer xs
    | x == '-'  = Symbol "-" : tokenizer xs
    | x == '*'  = Symbol "*" : tokenizer xs
    | x == '/'  = Symbol "/" : tokenizer xs
    | x == '('  = Symbol "(" : tokenizer xs
    | x == ')'  = Symbol ")" : tokenizer xs
    | x == '<'  =
        let (nx:nxs) = xs
        in case nx of '=' -> Symbol "<=" : tokenizer nxs
                      _   -> Symbol "<"  : tokenizer xs
    | x == '>'  =
        let (nx:nxs) = xs
        in case nx of '=' -> Symbol ">=" : tokenizer nxs
                      _   -> Symbol ">"  : tokenizer xs
    | x == '='  =
        let (nx:nxs) = xs
        in case nx of '=' -> Symbol "==" : tokenizer nxs
                      _   -> Symbol "="  : tokenizer xs
    | x == '!'  =
        let (nx:nxs) = xs
        in case nx of '=' -> Symbol "!=" : tokenizer nxs
                      _   -> error $ "tokenizer function failed: " ++ (show x) ++ (show nx) ++ " is not unexpected."
    | x == ','  = Symbol "," : tokenizer xs
    | x == ';'  = Symbol ";" : tokenizer xs
    | isDigit x =
        let len = charLength isDigit (x:xs)
        in Number (take len $ x:xs) : (tokenizer . drop len $ x:xs)
    | isLower x =
        let len = charLength isLower (x:xs)
        in Variable (take len $ x:xs) : (tokenizer . drop len $ x:xs)
    | otherwise   = error $ "tokenizer function failed: " ++ (show x) ++ " is not unexpected."

-- 与えられた文字列の先頭から、連続して関数fを満たす文字の個数を返す
-- charLength isDigit "250zzz" -> 3
-- charLength isDigit "qw12er3" -> 0
-- charLength isLower "250zzz" -> 0
-- charLength isLower "qw12er3" -> 2
charLength :: (Char -> Bool) -> String -> Int
charLength f "" = 0
charLength f (x:xs)
    | f x = 1 + charLength f xs
    | otherwise = 0
