module Lib
    ( genAssemblyCode
    ) where

import Data.List (elemIndex)
import Data.Char (isDigit, isLower, ord)

data ParseTree a = Empty | Leaf a | Tree a (ParseTree a) (ParseTree a) deriving (Show, Eq)
data Token = Number String | Symbol String | Variable Char deriving (Show, Eq)

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
    prologue
    ++
    (concat $ map genCode $ parser $ tokenizer text)
    ++
    ["  pop rax"]
    ++
    epilogue

prologue :: [String]
prologue =
    [
     "  push rbp",
     "  mov rbp, rsp",
     "  sub rsp, " ++ (show $ 8 * 26)
    ]

epilogue :: [String]
epilogue =
    [
     "  mov rsp, rbp",
     "  pop rbp",
     "  ret"
    ]


---- トークナイザ ----
tokenizer :: String -> [Token]
tokenizer ""        = []
tokenizer (x:xs)
    | x == '+'  = Symbol "+" : tokenizer xs
    | x == '-'  = Symbol "-" : tokenizer xs
    | x == '*'  = Symbol "*" : tokenizer xs
    | x == '/'  = Symbol "/" : tokenizer xs
    | x == '('  = Symbol "(" : tokenizer xs
    | x == ')'  = Symbol ")" : tokenizer xs
    | x == '='  =
        let (nx:nxs) = xs
        in case nx of '=' -> Symbol "==" : tokenizer nxs
                      _   -> Symbol "="  : tokenizer xs
    | x == '!'  =
        let (nx:nxs) = xs
        in case nx of '=' -> Symbol "!=" : tokenizer nxs
                      _   -> error $ "tokenizer function failed: " ++ (show x) ++ (show nx) ++ " is not unexpected."
    | x == ';'  = Symbol ";" : tokenizer xs
    | isDigit x =
        let len = numLength (x:xs)
        in Number (take len $ x:xs) : (tokenizer . drop len $ x:xs)
    | isLower x = Variable x : tokenizer xs
    | otherwise   = error $ "tokenizer function failed: " ++ (show x) ++ " is not unexpected."

-- 与えられた文字列の先頭に含まれる整数の長さを返す
-- numLength "250" -> 3
-- numLength "40zxc" -> 2
-- numLength "abc234" -> 0
-- numLength "q1w2e3r" -> 0
numLength :: String -> Int
numLength "" = 0
numLength (x:xs)
    | isDigit x = 1 + numLength xs
    | otherwise = 0


---- パーサ ----
parser :: [Token] -> [ParseTree Token]
parser xs = stmt xs

-- ParseTree: パースが完了した分の構文解析木
-- [Token]: これからパースする残りのトークン
stmt :: [Token] -> [ParseTree Token]
stmt [] = []
stmt xs =
    let (ptree, nxs) = opOrder14 xs
    in  case nxs of
        [] -> error "';' was not found at the end of a sentence."
        (x:_)
            | x == Symbol ";" -> ptree : (stmt $ tail nxs)
            | otherwise       -> error "stmt function failed."

-- operations: '='
-- RIGHT associative
opOrder14 :: [Token] -> (ParseTree Token, [Token])
opOrder14 xs = opOrder14' $ opOrder7 xs

opOrder14' :: (ParseTree Token, [Token]) -> (ParseTree Token, [Token])
opOrder14' (ptree, []) = (ptree, [])
opOrder14' (ptree, x:xs)
    | x == Symbol "=" =
        let (nptree, nxs) = opOrder7 xs
            (rptree, nnxs) = opOrder14' (nptree, nxs)
        in (Tree x ptree rptree, nnxs)
    | otherwise =
        (ptree, x:xs)

-- operations: '==', '!='
-- LEFT associative
opOrder7 :: [Token] -> (ParseTree Token, [Token])
opOrder7 xs = opOrder7' $ opOrder4 xs

opOrder7' :: (ParseTree Token, [Token]) -> (ParseTree Token, [Token])
opOrder7' (ptree, []) = (ptree, [])
opOrder7' (ptree, x:xs)
    | x == Symbol "==" || x == Symbol "!=" =
        let (rptree, nxs) = opOrder4 xs
            nptree = Tree x ptree rptree
        in  opOrder7' (nptree, nxs)
    | otherwise =
        (ptree, x:xs)

-- operations: '+', '-'
-- LEFT associative
opOrder4 :: [Token] -> (ParseTree Token, [Token])
opOrder4 xs = opOrder4' $ opOrder3 xs

opOrder4' :: (ParseTree Token, [Token]) -> (ParseTree Token, [Token])
opOrder4' (ptree, []) = (ptree, [])
opOrder4' (ptree, x:xs)
    | x == Symbol "+" || x == Symbol "-" =
        let (rptree, nxs) = opOrder3 xs
            nptree = Tree x ptree rptree
        in  opOrder4' (nptree, nxs)
    | otherwise =
        (ptree, x:xs)

-- operations: '*', '/'
-- LEFT associative
opOrder3 :: [Token] -> (ParseTree Token, [Token])
opOrder3 xs = opOrder3' $ opOrder1 xs

opOrder3' :: (ParseTree Token, [Token]) -> (ParseTree Token, [Token])
opOrder3' (ptree, []) = (ptree, [])
opOrder3' (ptree, x:xs)
    | x == Symbol "*" || x == Symbol "/" =
        let (rptree, nxs) = opOrder1 xs
            nptree = Tree x ptree rptree
        in  opOrder3' (nptree, nxs)
    | otherwise =
        (ptree, x:xs)

-- operations: ()
opOrder1 :: [Token] -> (ParseTree Token, [Token])
opOrder1 ((Symbol "(") : xs) =
    let (ptree, nxs) = opOrder14 xs  -- カッコで囲まれている部分のトークンをパースする
    in  case nxs of
    []                 -> error "There is no closing parenthesis."
    x:xxs
     | x == Symbol ")" -> (ptree, xxs)
     | otherwise       -> error $ "opOrder function failed: expected -> ')' , but actual -> " ++ (show x) ++ " ."
opOrder1 ((Number x) : xs)   = (Leaf (Number x), xs)
opOrder1 ((Variable x) : xs) = (Leaf (Variable x), xs)
opOrder1 x = error "opOrder function failed."


---- コード生成 ----
-- 構文木からアセンブリコードを生成する
genCode :: ParseTree Token -> [String]
genCode (Leaf (Number x)) = ["  push " ++ x]
genCode (Leaf x) =
    genLval (Leaf x)
    ++
    [
     "  pop rax",
     "  mov rax, [rax]",
     "  push rax"
    ]
genCode (Tree (Symbol "=") lptree rptree) =
    genLval lptree
    ++
    genCode rptree
    ++
    [
     "  pop rdi",
     "  pop rax",
     "  mov [rax], rdi",
     "  push rdi"
    ]
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
        "+"  -> ["  add rax, rdi"]
        "-"  -> ["  sub rax, rdi"]
        "*"  -> ["  mul rdi"]
        "/"  -> [
                 "  mov rdx, 0",
                 "  div rdi"
                ]
        "==" -> [
                 "  cmp rax, rdi",
                 "  sete al",
                 "  movzb rax, al"
                ]
        "!=" -> [
                 "  cmp rax, rdi",
                 "  setne al",
                 "  movzb rax, al"
                ]
        _    -> error $ "calcCode function failed: " ++ (show x) ++ " ."
    ++
    ["  push rax"]
genCode Empty = error "calcCode function failed."

--  左辺値のアドレスを push する
genLval :: ParseTree Token -> [String]
genLval (Leaf (Variable x)) =
    let offset = (ord x - ord 'a' + 1) * 8
    in
    [
     "  mov rax, rbp",
     "  sub rax, " ++ show offset ,
     "  push rax"
    ]
genLval _ = error "genLval function failed."
