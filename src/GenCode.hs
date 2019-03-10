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
    (concat $ map (\x -> x ++ ["  pop rax"]) $ map (\x -> fst $ genCode x vs 0) ptrees)
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
     "  sub rsp, " ++ show offset,
     "  and rsp, 0xfffffffffffffff0"    -- 16-byte alignment of the stack
    ]

epilogue :: [String]
epilogue =
    [
     "  mov rsp, rbp",
     "  pop rbp",
     "  ret"
    ]


-- <Input>
-- ParseTree Token: 生成対象の構文木
-- VariableScope: 使用されている変数のリスト
-- PushCount: 関数が呼ばれた時点で積まれているスタックの総数
-- <Output>
-- [String]: 生成したアセンブリコード
-- PushCount: 関数内で積んだスタックの総数

---- コード生成 ----
-- 構文木からアセンブリコードを生成する
genCode :: ParseTree Token -> VariableScope -> PushCount -> ([String], PushCount)
genCode (Leaf (Number x)) _ _ = (["  push " ++ x], 1)
genCode (Leaf (Variable x)) vs cnt1 =
    let (code, cnt2) = genLval (Leaf (Variable x)) vs cnt1
        ncode =
            code
            ++
            [
             "  pop rax",
             "  mov rax, [rax]",
             "  push rax"
            ]
    in (ncode, cnt2)
genCode (Tree (Symbol "=") lptree rptree) vs cnt1 =
    let (code1, cnt21) = genLval lptree vs cnt1
        (code2, cnt22) = genCode rptree vs (cnt1 + cnt21)
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
    in  (ncode, cnt21 + cnt22 - 1)
genCode (Tree (Symbol ",") lptree rptree) vs cnt1 =
    let (code1, cnt21) = genCode rptree vs cnt1  -- push from a last argument
        (code2, cnt22) = genCode lptree vs (cnt1 + cnt21)
    in  (code1 ++ code2, cnt21 + cnt22)
genCode (Tree (Symbol x) lptree rptree) vs cnt1 =
    let (code1, cnt21) = genCode lptree vs cnt1
        (code2, cnt22) = genCode rptree vs (cnt1 + cnt21)
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
    in  (ncode, cnt21 + cnt22 - 1)
genCode (Tree Function (Leaf (Variable x)) rptree) vs cnt1 =
    let registers = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
        (code, cnt2) = case rptree of
                           Empty -> ([], 0)                  -- No arguments
                           _     -> genCode rptree vs cnt1_  -- More than one argument
                           where cnt1_ = case cnt1 `mod` 2 == 0 of  -- 引数をpushする前にアラインメントを調整する必要がある。
                                             True  -> cnt1          -- （引数が7以上ある場合、calleeが自分のrbpを基準にしてcallerのframe内の値にアクセスするため、
                                             False -> cnt1 + 1      -- calleeとcallerのframe間にpaddingを挟めない）
                                                                    -- そのためここではアラインメントがなされたものとして、引数を評価するためのアセンブリコードを出力する。
        nargOnStack = max 0 (cnt2 - 6)
        fAligned = (cnt1 + nargOnStack) `mod` 2 == 0
        ncode = case fAligned of
                    True  -> []
                    False -> ["  sub rsp, 8"]
                ++
                code
                ++
                (concat $ take (min cnt2 6) $ map (\reg -> ["  pop " ++ reg]) registers)
                ++
                ["  call " ++ x]
                ++
                case nargOnStack <= 0 of
                    True  -> []
                    False -> ["  add rsp, 0x" ++ showHex (nargOnStack * 8) ""]    -- 関数呼び出し後はスタック上にある第7引数以降を破棄する
                ++
                case fAligned of
                    True  -> []
                    False -> ["  add rsp, 8"]
                ++
                ["  push rax"]
    in  (ncode, 1)
genCode Empty _ _ = error "calcCode function failed."

--  左辺値のアドレスを push する
genLval :: ParseTree Token -> VariableScope -> PushCount -> ([String], PushCount)
genLval (Leaf (Variable x)) vs _ =
    -- let offset = (ord x - ord 'a' + 1) * 8
    let offset = vs Map.! x
        code =
            [
             "  mov rax, rbp",
             "  sub rax, " ++ show offset,
             "  push rax"
            ]
    in (code, 1)
genLval _ _ _ = error "genLval function failed."
