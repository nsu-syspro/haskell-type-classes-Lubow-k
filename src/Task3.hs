{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Task3 where

import Task1 (Parse (..))
import Task2 (Eval (..), Expr, evalExpr, parseTokens)

import Data.Monoid (Any(..))
import Data.Maybe (fromMaybe)
import Data.List (nub)

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
--
solveSAT :: String -> Maybe Bool
solveSAT input = do
    let tokens       = words input  
    let combinations = allCombinations (getVars tokens) 
    expr <- parseTokens [] tokens :: Maybe (Expr Bool BoolOp)
    Just(anyMaybeTrue (map (`evalExpr` expr ) combinations))

anyMaybeTrue :: [Maybe Bool] -> Bool
anyMaybeTrue = getAny . foldMap maybeBoolToAny

-- Noting -> false 
maybeBoolToAny :: Maybe Bool -> Any
maybeBoolToAny  = Any . fromMaybe False

getVars :: [String] -> [String]
getVars = nub . filter (`notElem` ["and", "or", "xor"])

listComprehention :: [a] -> [b] -> [(a, b)]
listComprehention xs ys = [(x, y) | x <- xs, y <- ys]

allCombinations :: [String] -> [[(String, Bool)]]
allCombinations []     = [[]]
allCombinations (v:vs) = [(v, b) : rest | b <- [True, False], rest <- allCombinations vs]


instance Parse Bool where
  parse s = case s of
    "true"  -> Just True
    "false" -> Just False
    _       -> Nothing

data BoolOp = And | Or | Xor
  deriving (Show)

instance Parse BoolOp where
  parse s = case s of
    "and" -> Just And
    "or"  -> Just Or
    "xor" -> Just Xor
    _     -> Nothing

instance Eval Bool BoolOp where
  evalBinOp And = (&&)
  evalBinOp Or  = (||)
  evalBinOp Xor = (/=)