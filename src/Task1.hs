{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}
-- The above pragma enables all warnings

module Task1 where

import Prelude (Integer, Show, String, Maybe(Just, Nothing), (+), (*), (.), reads, words, fmap, otherwise)

-- * Expression data type

-- | Representation of integer arithmetic expressions comprising
-- - Literals of type 'a'
-- - Binary operations 'Add' and 'Mul'
data IExpr =
    Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving Show

-- * Evaluation

-- | Evaluates given 'IExpr'
--
-- Usage example:
--
-- >>> evalIExpr (Lit 2)
-- 2
-- >>> evalIExpr (Add (Lit 2) (Lit 3))
-- 5
-- >>> evalIExpr (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- 9
--
evalIExpr :: IExpr -> Integer
evalIExpr (Lit x)   = x
evalIExpr (Add l r) = evalIExpr l + evalIExpr r
evalIExpr (Mul l r) = evalIExpr l * evalIExpr r


-- * Parsing

-- | Class of parseable types
class Parse a where
  -- | Parses value 'a' from given string
  -- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
  parse :: String -> Maybe a

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe IExpr
-- Just (Lit 2)
-- >>> parse "2 3 +" :: Maybe IExpr
-- Just (Add (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe IExpr
-- Just (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe IExpr
-- Nothing
-- >>> parse "2 3" :: Maybe IExpr
-- Nothing
--
instance Parse IExpr where
  parse :: String -> Maybe IExpr
  parse = parseTokens [] . words where 
    parseTokens :: [IExpr] -> [String] -> Maybe IExpr
    parseTokens stack []
      | [result] <- stack = Just result
      | otherwise         = Nothing
    parseTokens stack (t:tokens) =
        case t of
            "+"  -> apply Add stack tokens
            "*"  -> apply Mul stack tokens
            s    -> case reads s of
                    [(num, "")] -> parseTokens (Lit num : stack) tokens
                    _           -> Nothing 

    apply :: (IExpr -> IExpr -> IExpr) -> [IExpr] -> [String] -> Maybe IExpr
    apply op (y:x:rest) tokens = parseTokens (op x y : rest) tokens 
    apply _ _ _                = Nothing 


-- * Evaluation with parsing

-- | Parses given expression in Reverse Polish Notation and evaluates it
--
-- Returns 'Nothing' in case the expression could not be parsed.
--
-- Usage example:
--
-- >>> evaluateIExpr "2"
-- Just 2
-- >>> evaluateIExpr "2 3 +"
-- Just 5
-- >>> evaluateIExpr "3 2 * 3 +"
-- Just 9
-- >>> evaluateIExpr "2 +"
-- Nothing
-- >>> evaluateIExpr "2 3"
-- Nothing
--
evaluateIExpr :: String -> Maybe Integer
evaluateIExpr = fmap evalIExpr . parse
