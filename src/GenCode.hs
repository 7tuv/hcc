module GenCode
    ( genAssemblyCode
    ) where

import qualified Data.Map as Map
import Data.Char (ord)
import DataType
import Tokenizer (tokenizer)
import Parser (parser)


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
