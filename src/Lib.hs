module Lib
    ( genAssemblyCode
    ) where

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
     "main:",
     "  mov rax, " ++ text,
     "  ret"
    ]