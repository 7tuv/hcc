module Lib
    ( genAssemblyCode
    ) where

import Data.List (elemIndex)
import Data.Char (isDigit, isLower, ord)
import qualified Data.Map as Map

data ParseTree a = Empty | Leaf a | Tree a (ParseTree a) (ParseTree a) deriving (Show, Eq)
data Token = Number String | Symbol String | Variable String deriving (Show, Eq)
type VariableScope = Map.Map String Int

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
    let (ptrees, vs) = parser $ tokenizer text
    in
    [
     "main:"
    ]
    ++
    prologue vs
    ++
    (concat $ map (\x -> genCode x vs) ptrees)
    ++
    ["  pop rax"]
    ++
    epilogue

prologue :: VariableScope -> [String]
prologue vs =
    let xs = Map.elems vs
        offset = case xs of [] -> 0
                            _  -> foldl1 max xs
    in
    [
     "  push rbp",
     "  mov rbp, rsp",
     "  sub rsp, " ++ show offset
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


---- パーサ ----
parser :: [Token] -> ([ParseTree Token], VariableScope)
parser xs = stmt xs Map.empty

-- ParseTree: パースが完了した分の構文解析木
-- [Token]: これからパースする残りのトークン
stmt :: [Token] -> VariableScope -> ([ParseTree Token], VariableScope)
stmt [] vs = ([], vs)
stmt xs vs =
    let (ptree, nxs, nvs) = opOrder14 xs vs
    in  case nxs of
        [] -> error "';' was not found at the end of a sentence."
        (x:_)
            | x == Symbol ";" -> let (nptree, nnvs) = stmt (tail nxs) nvs
                                 in  (ptree : nptree, nnvs)
            | otherwise       -> error "stmt function failed."

-- operations: '='
-- RIGHT associative
opOrder14 :: [Token] -> VariableScope -> (ParseTree Token, [Token], VariableScope)
opOrder14 xs vs = opOrder14' $ opOrder7 xs vs

opOrder14' :: (ParseTree Token, [Token], VariableScope) -> (ParseTree Token, [Token], VariableScope)
opOrder14' (ptree, [], vs) = (ptree, [], vs)
opOrder14' (ptree, x:xs, vs)
    | x == Symbol "=" =
        let (nptree, nxs, nvs) = opOrder7 xs vs
            (rptree, nnxs, nnvs) = opOrder14' (nptree, nxs, nvs)
        in (Tree x ptree rptree, nnxs, nnvs)
    | otherwise =
        (ptree, x:xs, vs)

-- operations: '==', '!='
-- LEFT associative
opOrder7 :: [Token] -> VariableScope -> (ParseTree Token, [Token], VariableScope)
opOrder7 xs vs = opOrder7' $ opOrder6 xs vs

opOrder7' :: (ParseTree Token, [Token], VariableScope) -> (ParseTree Token, [Token], VariableScope)
opOrder7' (ptree, [], vs) = (ptree, [], vs)
opOrder7' (ptree, x:xs, vs)
    | x == Symbol "==" || x == Symbol "!=" =
        let (rptree, nxs, nvs) = opOrder6 xs vs
            nptree = Tree x ptree rptree
        in  opOrder7' (nptree, nxs, nvs)
    | otherwise =
        (ptree, x:xs, vs)

-- operations: '<', '>', '<=', '>='
-- LEFT associative
opOrder6 :: [Token] -> VariableScope -> (ParseTree Token, [Token], VariableScope)
opOrder6 xs vs = opOrder6' $ opOrder4 xs vs

opOrder6' :: (ParseTree Token, [Token], VariableScope) -> (ParseTree Token, [Token], VariableScope)
opOrder6' (ptree, [], vs) = (ptree, [], vs)
opOrder6' (ptree, x:xs, vs)
    | x == Symbol "<" || x == Symbol ">" || x == Symbol "<=" || x == Symbol ">=" =
        let (rptree, nxs, nvs) = opOrder4 xs vs
            nptree = Tree x ptree rptree
        in  opOrder6' (nptree, nxs, nvs)
    | otherwise =
        (ptree, x:xs, vs)

-- operations: '+', '-'
-- LEFT associative
opOrder4 :: [Token] -> VariableScope -> (ParseTree Token, [Token], VariableScope)
opOrder4 xs vs = opOrder4' $ opOrder3 xs vs

opOrder4' :: (ParseTree Token, [Token], VariableScope) -> (ParseTree Token, [Token], VariableScope)
opOrder4' (ptree, [], vs) = (ptree, [], vs)
opOrder4' (ptree, x:xs, vs)
    | x == Symbol "+" || x == Symbol "-" =
        let (rptree, nxs, nvs) = opOrder3 xs vs
            nptree = Tree x ptree rptree
        in  opOrder4' (nptree, nxs, nvs)
    | otherwise =
        (ptree, x:xs, vs)

-- operations: '*', '/'
-- LEFT associative
opOrder3 :: [Token] -> VariableScope -> (ParseTree Token, [Token], VariableScope)
opOrder3 xs vs = opOrder3' $ opOrder1 xs vs

opOrder3' :: (ParseTree Token, [Token], VariableScope) -> (ParseTree Token, [Token], VariableScope)
opOrder3' (ptree, [], vs) = (ptree, [], vs)
opOrder3' (ptree, x:xs, vs)
    | x == Symbol "*" || x == Symbol "/" =
        let (rptree, nxs, nvs) = opOrder1 xs vs
            nptree = Tree x ptree rptree
        in  opOrder3' (nptree, nxs, nvs)
    | otherwise =
        (ptree, x:xs, vs)

-- operations: ()
opOrder1 :: [Token] -> VariableScope -> (ParseTree Token, [Token], VariableScope)
opOrder1 ((Symbol "(") : xs) vs =
    let (ptree, nxs, nvs) = opOrder14 xs vs  -- カッコで囲まれている部分のトークンをパースする
    in  case nxs of
            []                 -> error "There is no closing parenthesis."
            x:xxs
             | x == Symbol ")" -> (ptree, xxs, nvs)
             | otherwise       -> error $ "opOrder function failed: expected -> ')' , but actual -> " ++ (show x) ++ " ."
opOrder1 ((Number x) : xs) vs   = (Leaf (Number x), xs, vs)
opOrder1 ((Variable x) : xs) vs = let size = case Map.elems vs of
                                                 [] -> 0
                                                 xs -> foldl1 max xs
                                      nvs  = case Map.member x vs of
                                                 True  -> vs
                                                 False -> Map.insert x (size+8) vs
                                  in  (Leaf (Variable x), xs, nvs)
opOrder1 x vs = error "opOrder function failed."


---- コード生成 ----
-- 構文木からアセンブリコードを生成する
genCode :: ParseTree Token -> VariableScope -> [String]
genCode (Leaf (Number x)) _ = ["  push " ++ x]
genCode (Leaf (Variable x)) vs =
    genLval (Leaf (Variable x)) vs
    ++
    [
     "  pop rax",
     "  mov rax, [rax]",
     "  push rax"
    ]
genCode (Tree (Symbol "=") lptree rptree) vs =
    genLval lptree vs
    ++
    genCode rptree vs
    ++
    [
     "  pop rdi",   -- value of "genCode rptree"
     "  pop rax",   -- value of "genCode lptree" (address)
     "  mov [rax], rdi",
     "  push rdi"
    ]
genCode (Tree (Symbol x) lptree rptree) vs =
    genCode lptree vs
    ++
    genCode rptree vs
    ++
    [
     "  pop rdi",   -- value of "genCode rptree"
     "  pop rax"    -- value of "genCode lptree"
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
        "<"  -> [
                 "  cmp rax, rdi",
                 "  sets al",
                 "  movzb rax, al"
                ]
        ">=" -> [
                 "  cmp rax, rdi",
                 "  setns al",
                 "  movzb rax, al"
                ]
        ">"  -> [
                 "  cmp rdi, rax",
                 "  sets al",
                 "  movzb rax, al"
                ]
        "<=" -> [
                 "  cmp rdi, rax",
                 "  setns al",
                 "  movzb rax, al"
                ]
        _    -> error $ "calcCode function failed: " ++ (show x) ++ " ."
    ++
    ["  push rax"]
genCode Empty vs = error "calcCode function failed."

--  左辺値のアドレスを push する
genLval :: ParseTree Token -> VariableScope -> [String]
genLval (Leaf (Variable x)) vs =
    -- let offset = (ord x - ord 'a' + 1) * 8
    let offset = vs Map.! x
    in
    [
     "  mov rax, rbp",
     "  sub rax, " ++ show offset ,
     "  push rax"
    ]
genLval _ vs = error "genLval function failed."
