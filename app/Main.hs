module Main where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
    args <- getArgs
    if 1 /= length args
    then
        putStrLn "1 argument is requierd."
    else do
        let text = head args
        mapM_ putStrLn (genAssemblyCode text)
