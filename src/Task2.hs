{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}


module Task2 where

import Task1 (Parse, Parse(..))

-- * Expression data type

-- | Generalized representation of expressions comprising
-- - Literals of type 'a'
-- - Variables with arbitrary 'String' names
-- - Binary operations of type 'op'
data Expr a op =
    Lit a
  | Var String
  | BinOp op (Expr a op) (Expr a op)
  deriving Show

-- | Integer binary operations
data IntOp = Add | Mul | Sub
  deriving Show

-- * Parsing

instance Parse Integer where 
  parse :: String -> Maybe Integer
  parse s = case reads s of
            [(num, "")] -> Just num
            _           -> Nothing 


instance Parse IntOp where 
  parse :: String -> Maybe IntOp
  parse s = case s of
    "+" -> Just Add
    "*" -> Just Mul
    "-" -> Just Sub
    _   -> Nothing


-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe (Expr Integer IntOp)
-- Just (Lit 2)
-- >>> parse "2 3 -" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Sub (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Add (BinOp Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe (Expr Integer IntOp)
-- Nothing
-- >>> parse "2 3" :: Maybe (Expr Integer IntOp)
-- Nothing
--

instance (Parse a, Parse op) => Parse (Expr a op) where

  parse :: (Parse a, Parse op) => String -> Maybe (Expr a op)
  parse = parseTokens [] . words  

parseTokens :: (Parse a, Parse op) => [Expr a op] -> [String] -> Maybe (Expr a op)
parseTokens stack [] 
  | [result] <- stack = Just result
  | otherwise         = Nothing
parseTokens stack (t:tokens) = 
  case parse t of
    Just a  -> parseTokens (Lit a : stack) tokens
    Nothing -> case stack of
      (l : r : es) -> case parse t of
        Just op -> parseTokens (BinOp op r l : es) tokens
        Nothing -> parseVar stack t tokens 
      _         -> parseVar stack t tokens 

parseVar :: (Parse a, Parse op) => [Expr a op] -> String -> [String] -> Maybe (Expr a op)
parseVar stack t = parseTokens (Var t : stack) 

-- * Evaluation

-- | Class of evaluatable types
class Eval a op where
  -- | Evaluates given binary operation with provided arguments
  evalBinOp :: op -> a -> a -> a


instance Eval Integer IntOp where
  evalBinOp op = case op of
    Add -> (+)
    Mul -> (*)
    Sub -> (-)

-- | Evaluates given 'Expr' using given association list of variable values
--
-- Returns 'Nothing' in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evalExpr [] (Lit 2 :: Expr Integer IntOp)
-- Just 2
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "x")) :: Maybe Integer
-- Just 5
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "y")) :: Maybe Integer
-- Nothing
--
evalExpr :: (Eval a op) => [(String, a)] -> Expr a op -> Maybe a
evalExpr _ (Lit x)               = Just x 
evalExpr vars (Var v)            = lookup v vars
evalExpr vars (BinOp op l r)     = case (evalExpr vars l, evalExpr vars r) of
  (Just correctL, Just correctR) -> Just (evalBinOp op correctL correctR)
  _                              -> Nothing


-- | Parses given integer expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evaluateInteger [] "2"
-- Just 2
-- >>> evaluateInteger [("x", 3)] "2 x -"
-- Just (-1)
-- >>> evaluateInteger [("x", 3)] "2 y -"
-- Nothing
-- >>> evaluateInteger [] "3 2 * 3 +"
-- Just 9
-- >>> evaluateInteger [] "2 +"
-- Nothing
-- >>> evaluateInteger [] "2 3"
-- Nothing
--
evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
evaluateInteger = evaluate reifyInteger 


-- | Parses given expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- The 'Reify' function is required to reconcile generic type
-- of intermediate 'Expr' expression with concrete type using 'a' and 'op'.
--
evaluate :: (Eval a op, Parse a, Parse op) => Reify a op -> [(String, a)] -> String -> Maybe a
evaluate reify m s = case parse s of
  Just e  -> evalExpr m (reify e)
  Nothing -> Nothing

-- * Helpers

-- | Helper type for specifying 'Expr' with
-- concrete 'a' and 'op' in generic context
type Reify a op = Expr a op -> Expr a op

-- | Helper for specifying 'Expr' with 'Integer' and 'IntOp' in generic context
reifyInteger :: Reify Integer IntOp
reifyInteger = id
