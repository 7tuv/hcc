module Lib
    ( genAssemblyCode
    ) where

import Data.Char (isDigit)

data Symbol = Number String | Plus | Minus deriving (Show)

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
    (calcCode $ parse text)
    ++
    [
     "  ret"
    ]

calcCode :: [Symbol] -> [String]
calcCode [] = []
calcCode (Number x : xs)         = ("  mov rax, " ++ x) : calcCode xs
calcCode (Plus  : Number x : xs) = ("  add rax, " ++ x) : calcCode xs
calcCode (Minus : Number x : xs) = ("  sub rax, " ++ x) : calcCode xs
calcCode x = error "calcCode function failed"

parse :: String -> [Symbol]
parse ""        = []
parse (x:xs)
    | x == '+'  = Plus  : parse xs
    | x == '-'  = Minus : parse xs
    | isDigit x =
        let len = numLength (x:xs)
        in Number (take len $ x:xs) : (parse . drop len $ x:xs)
    | otherwise   = error "parse function failed"

numLength :: String -> Int
numLength "" = 0
numLength (x:xs)
    | isDigit x = 1 + numLength xs
    | otherwise = 0
