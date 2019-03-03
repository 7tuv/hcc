module Parser
    ( parser
    ) where

import qualified Data.Map as Map
import DataType

---- パーサ ----
parser :: [Token] -> ([ParseTree Token], VariableScope)
parser xs = stmt xs Map.empty

-- ParseTree     : パースが完了した分の構文解析木
-- [Token]       : これからパースする残りのトークン
-- VariableScope : パースが完了した分までで使用されている変数の一覧
stmt :: [Token] -> VariableScope -> ([ParseTree Token], VariableScope)
stmt [] vs = ([], vs)
stmt xs vs =
    let (ptree, nxs, nvs) = opOrder15 xs vs
    in  case nxs of
        [] -> error "';' was not found at the end of a sentence."
        (x:_)
            | x == Symbol ";" -> let (nptree, nnvs) = stmt (tail nxs) nvs
                                 in  (ptree : nptree, nnvs)
            | otherwise       -> error "stmt function failed."

-- operations: ','
-- LEFT associative
opOrder15 :: [Token] -> VariableScope -> (ParseTree Token, [Token], VariableScope)
opOrder15 xs vs = opOrder15' $ opOrder14 xs vs

opOrder15' :: (ParseTree Token, [Token], VariableScope) -> (ParseTree Token, [Token], VariableScope)
opOrder15' (ptree, [], vs) = (ptree, [], vs)
opOrder15' (ptree, x:xs, vs)
    | x == Symbol "," =
        let (rptree, nxs, nvs) = opOrder14 xs vs
            nptree = Tree x ptree rptree
        in  opOrder15' (nptree, nxs, nvs)
    | otherwise =
        (ptree, x:xs, vs)

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
    let (ptree, nxs, nvs) = opOrder15 xs vs  -- カッコで囲まれている部分のトークンをパースする
    in  case nxs of
            []                 -> error "There is no closing parenthesis."
            x:xxs
             | x == Symbol ")" -> (ptree, xxs, nvs)
             | otherwise       -> error $ "opOrder function failed: expected -> ')' , but actual -> " ++ (show x) ++ " ."
opOrder1 ((Number x) : xs) vs   = (Leaf (Number x), xs, vs)
opOrder1 ((Variable x) : xs) vs =
    case xs of
        Symbol "(" : xxs   -- function
            -> case head xxs of
                   Symbol ")" -> let nptree = Tree Function (Leaf (Variable x)) Empty  -- No arguments
                                 in  (nptree, tail xxs, vs)
                   _          -> let (ptree, nxs, nvs) = opOrder1 xs vs                -- More than one argument
                                     nptree = Tree Function (Leaf (Variable x)) ptree
                                 in  (nptree, nxs, nvs)
        _   -> let size =  -- variable
                       case Map.elems vs of
                           [] -> 0
                           ys -> foldl1 max ys
                   nvs  =
                       case Map.member x vs of
                           True  -> vs
                           False -> Map.insert x (size + 8) vs
               in  (Leaf (Variable x), xs, nvs)
opOrder1 x vs = error "opOrder function failed."
