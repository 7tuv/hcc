module DataType
    ( ParseTree(..),
      Token(..),
      VariableScope,
    ) where

import qualified Data.Map as Map

data ParseTree a = Empty | Leaf a | Tree a (ParseTree a) (ParseTree a) deriving (Show, Eq)
data Token = Number String | Symbol String | Variable String | Function deriving (Show, Eq)
type VariableScope = Map.Map String Int