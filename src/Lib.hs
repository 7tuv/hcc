module Lib
    ( genAssemblyCode
    ) where

import Data.List (elemIndex)
import Data.Char (isDigit)

data ParseTree a = Empty | Leaf a | Tree a (ParseTree a) (ParseTree a) deriving (Show, Eq)
data Token = Number String | Symbol Char deriving (Show, Eq)

genAssemblyCode :: String -> [String]
genAssemblyCode text =
    concat [assemblyCodeHead, assemblyCodeBody text]

assemblyCodeHead :: [String]
assemblyCodeHead =
    [
     ".intel_syntax noprefix",
     ".global main"
    ]

assemblyCodeBody :: String -> [String]
assemblyCodeBody text =
    [
     "main:"
    ]
    ++
    (genCode $ parser $ tokenizer text)
    ++
    [
     "  pop rax",
     "  ret"
    ]

tokenizer :: String -> [Token]
tokenizer ""        = []
tokenizer (x:xs)
    | x == '+'  = Symbol '+' : tokenizer xs
    | x == '-'  = Symbol '-' : tokenizer xs
    | x == '*'  = Symbol '*' : tokenizer xs
    | x == '/'  = Symbol '/' : tokenizer xs
    | x == '('  = Symbol '(' : tokenizer xs
    | x == ')'  = Symbol ')' : tokenizer xs
    | isDigit x =
        let len = numLength (x:xs)
        in Number (take len $ x:xs) : (tokenizer . drop len $ x:xs)
    | otherwise   = error "tokenizer function failed"

numLength :: String -> Int
numLength "" = 0
numLength (x:xs)
    | isDigit x = 1 + numLength xs
    | otherwise = 0

parser :: [Token] -> ParseTree Token
parser xs = let result = opOrder4 (Empty, xs)
            in case result of
                (ptree, xxs)
                 | xxs /= [] -> error "parser function failed"
                 | otherwise -> ptree

-- ParseTree: パースが完了した分の構文解析木
-- [Token]: これからパースする残りのトークン

-- operations: '+', '-'
-- LEFT assosiative
opOrder4 :: (ParseTree Token, [Token]) -> (ParseTree Token, [Token])
opOrder4 (ptree, xs) = opOrder4' $ opOrder3 (ptree, xs)

opOrder4' :: (ParseTree Token, [Token]) -> (ParseTree Token, [Token])
opOrder4' (ptree, []) = (ptree, [])
opOrder4' (ptree, x:xs)
    | x == Symbol '+' || x == Symbol '-' =
        let (rptree, nxs) = opOrder3 (ptree, xs)
            nptree = Tree x ptree rptree
        in  opOrder4' (nptree, nxs)
    | otherwise =
        (ptree, x:xs)

-- operations: '*', '/'
-- LEFT assosiative
opOrder3 :: (ParseTree Token, [Token]) -> (ParseTree Token, [Token])
opOrder3 (ptree, xs) = opOrder3' $ opOrder1 (ptree, xs)

opOrder3' :: (ParseTree Token, [Token]) -> (ParseTree Token, [Token])
opOrder3' (ptree, []) = (ptree, [])
opOrder3' (ptree, x:xs)
    | x == Symbol '*' || x == Symbol '/' =
        let (rptree, nxs) = opOrder1 (ptree, xs)
            nptree = Tree x ptree rptree
        in  opOrder3' (nptree, nxs)
    | otherwise =
        (ptree, x:xs)

-- operations: ()
opOrder1 :: (ParseTree Token, [Token]) -> (ParseTree Token, [Token])
opOrder1 (ptree, (Symbol '(') : xs) = let (nptree, nxs) = opOrder4 (Empty, xs)
                                      in  case nxs of
                                            []                 -> error "There is no closing parenthesis"
                                            x:xxs
                                             | x == Symbol ')' -> (nptree, xxs)
                                             | otherwise       -> error "opOrder function failed"
opOrder1 (ptree, (Number x) : xs)   = (Leaf (Number x), xs)
opOrder1 x = error "opOrder function failed"

-- 構文木からアセンブリコードを生成する
genCode :: ParseTree Token -> [String]
genCode (Leaf (Number x)) = ["  push " ++ x]
genCode (Tree (Symbol x) lptree rptree) =
    genCode lptree
    ++
    genCode rptree
    ++
    [
     "  pop rdi",
     "  pop rax"
    ]
    ++
    case x of
        '+' ->  ["  add rax, rdi"]
        '-' ->  ["  sub rax, rdi"]
        '*' ->  ["  mul rdi"]
        '/' ->  [
                 "  mov rdx, 0",
                 "  div rdi"
                ]
        _   -> error $ "calcCode function failed: " ++ [x] ++ " is invalid operator" 
    ++
    ["  push rax"]
genCode Empty = error "calcCode function failed"
