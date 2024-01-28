module DataType
    ( ParseTree(..),
      Token(..),
      VariableScope,
    ) where

import qualified Data.Map as Map

data ParseTree a = Empty | Leaf a | Tree a (ParseTree a) (ParseTree a) deriving (Show, Eq)
data Token = Number String | Symbol String | Variable String | Function deriving (Show, Eq)
-- 変数(String)がrbpからどれだけ離れた場所(Int)に格納されているか
-- Intはスタックを積む方向に + になっている。（アドレス的には - 方向）
type VariableScope = Map.Map String Int
