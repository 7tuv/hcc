module Lib
    ( output
    ) where

import System.Environment (getArgs)

output :: IO ()
output = do
    args <- getArgs
    if 1 /= length args
    then
        putStrLn "1 argument is requierd."
    else do
        putStrLn ".intel_syntax noprefix"
        putStrLn ".global main"
        putStrLn "main:"
        putStrLn $ "  mov rax, " ++ head args
        putStrLn "  ret"
