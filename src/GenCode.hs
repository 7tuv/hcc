module GenCode
    ( genAssemblyCode
    ) where

import Numeric (showHex)
import qualified Data.Map as Map
import Data.Char (ord)
import DataType
import Tokenizer (tokenizer)
import Parser (parser)

type PushCount = Int

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
    (concat $ map (\x -> fst $ genCode x vs) ptrees)
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
genCode :: ParseTree Token -> VariableScope -> ([String], PushCount)
genCode (Leaf (Number x)) _ = (["  push " ++ x], 1)
genCode (Leaf (Variable x)) vs =
    let (code, cnt) = genLval (Leaf (Variable x)) vs
        ncode =
            code
            ++
            [
             "  pop rax",
             "  mov rax, [rax]",
             "  push rax"
            ]
    in (ncode, cnt)
genCode (Tree (Symbol "=") lptree rptree) vs =
    let (code1, cnt1) = genLval lptree vs
        (code2, cnt2) = genCode rptree vs
        ncode = code1
                ++
                code2
                ++
                [
                 "  pop rdi",   -- value of "genCode rptree"
                 "  pop rax",   -- value of "genCode lptree" (address)
                 "  mov [rax], rdi",
                 "  push rdi"
                ]
    in  (ncode, cnt1 + cnt2 - 1)
genCode (Tree (Symbol ",") lptree rptree) vs =
    let (code1, cnt1) = genCode rptree vs  -- push from a last argument
        (code2, cnt2) = genCode lptree vs
    in  (code1 ++ code2, cnt1 + cnt2)
genCode (Tree (Symbol x) lptree rptree) vs =
    let (code1, cnt1) = genCode lptree vs
        (code2, cnt2) = genCode rptree vs
        ncode = code1
                ++
                code2
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
    in  (ncode, cnt1 + cnt2 - 1)
genCode (Tree Function (Leaf (Variable x)) rptree) vs =
    let registers = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
        (code, cnt) = case rptree of
                          Empty -> ([], 0)            -- No arguments
                          _     -> genCode rptree vs  -- More than one argument
        ncode = code
                ++
                (concat $ take (min cnt 6) $ map (\reg -> ["  pop rax", "  mov " ++ reg ++ ", rax"]) registers)
                ++
                ["  call " ++ x]
                ++
                case cnt <= 6 of
                    True  -> []
                    False -> ["  add rsp, 0x" ++ showHex ((cnt - 6) * 8) ""]    -- 第7引数以降は使わないので無視するため
                ++
                ["  push rax"]
    in  (ncode, 1)
genCode Empty vs = error "calcCode function failed."

--  左辺値のアドレスを push する
genLval :: ParseTree Token -> VariableScope -> ([String], PushCount)
genLval (Leaf (Variable x)) vs =
    -- let offset = (ord x - ord 'a' + 1) * 8
    let offset = vs Map.! x
        code =
            [
             "  mov rax, rbp",
             "  sub rax, " ++ show offset,
             "  push rax"
            ]
    in (code, 1)
genLval _ vs = error "genLval function failed."
