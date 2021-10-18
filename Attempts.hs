-- Some stuff that I'm writing in a separate file so I don't
-- Mess up what already exists.
-- The idea is to use the DPLL algorithm.

import Control.Applicative -- standard lib: ok
import Data.Set (Set) -- not standard: must check if ok w/ prof
import qualified Data.Set as Set
import Data.Maybe -- standard lib: ok

-- I really like this implementation, I found a little tutorial
-- It's simple and quite easy to understand imo
-- Needs to be adapted to be merged with the rest though

data Expr = Var Char
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Cst Bool -- cst = const; this is just to avoid ambiguity
  deriving (Show, Eq)

-- it is useful to be able to write any boolean expression in CNF format
-- We want to distribute dijunctions over conjunctions
-- This requires the application of DeMorgan's Laws

-- DeMorgan's laws:
-- not (A and B) = (not A) or (not B)
-- not (A or B) = (not A) and (not B)

-- Step 1: remove negations on anything except literals (eg we can
-- have [not A] but we cannot have [not (A or B)])

removeNeg :: Expr -> Expr
removeNeg expr =
  case expr of
    -- double negations
    Not (Not x) -> removeNeg x

    -- DeMorgan's laws
    -- $ is the infix application; avoids using extra parentheses
    -- i.e. f (g x) == f $ g x. Defined as:
    -- ($) :: (a -> b) -> (a -> b)
    -- f $ x = f x
    Not (And x y) -> Or (removeNeg $ Not x) (removeNeg $ Not y)
    Not (Or x y) -> And (removeNeg $ Not x) (removeNeg $ Not y)

    -- Cstants
    Not (Cst b) -> Cst (not b)

    -- apply recursion
    Not x -> Not (removeNeg x)
    And x y -> And (removeNeg x) (removeNeg y)
    Or x y -> Or (removeNeg x) (removeNeg y)
    x -> x

-- Step 2: rearrange the 'and's and 'or's
-- This means distributing 'or' over 'and' such that
-- A or (B and C) becomes (A or B) and (B or C)

distrAnd :: Expr -> Expr
distrAnd expr =
  case expr of
    Or x (And y z) -> And (Or (distrAnd x) (distrAnd y)) (Or (distrAnd x) (distrAnd z))
    Or (And x y) z -> And (Or (distrAnd z) (distrAnd x)) (Or (distrAnd z) (distrAnd y))

    -- apply recursion as before
    Or x y ->  Or (distrAnd x) (distrAnd y)
    And x y -> And (distrAnd x) (distrAnd y)
    Not x -> Not (distrAnd x)
    x -> x

-- Step 3: convert to CNF; we checked that we have stopped changing
-- things by comparing the result against the input
toCNF :: Expr -> Expr
toCNF expr =
  if new == expr
  then expr -- we are done, we have stopped changing things
  else toCNF new -- otherwise, run it again

  where new = distrAnd (removeNeg expr)

-- Improving efficiency compared to the naive algorithm
-- PART 1 - Eliminating literals
-- If some literal always appears as the same polarity (eg always true)
-- then we can immediately determine its truth value
-- Additionally, if a literal is alone, we can also immediately determine
-- its truth value
-- Generally if it is of positive polarity we can say it is true,
-- and false otherwise.

-- A function to get all literals from an expression
-- We use sets and operations on sets to help manage this
getLiterals :: Expr -> Set Char
getLiterals (Var v) = Set.singleton v
getLiterals (Not e) = getLiterals e
getLiterals (And x y) = Set.union (getLiterals x) (getLiterals y)
getLiterals (Or x y) = Set.union (getLiterals x) (getLiterals y)
getLiterals _ = Set.empty

-- if the polarity is positive, then the var is never negated
-- if is negative, it is always negated
-- if it is mixed, it is sometimes negated, sometimes not
data Polarity = Pos | Neg | Mix deriving (Show, Eq)

-- Determine the polarity of a literal, return Nothing if
-- literal is not in the expression
getPolarity :: Expr -> Char -> Maybe Polarity

-- check why we use 'just' and 'maybe'
getPolarity (Var v) vp
  | v == vp   = Just Pos
  | otherwise = Nothing

getPolarity (Not (Var v)) vp
  | v == vp   = Just Neg
  | otherwise = Nothing

getPolarity ex v =
  case ex of
    And x y -> combinePol [x,y]
    Or x y -> combinePol [x, y]
    Not x -> error $ "The variable you are negating is not in the CNF " ++ show x
    Cst _ -> Nothing
  where
    combinePol e =
      let pols = mapMaybe (flip getPolarity v) e
    -- mapMaybe is a version of map which can throw out elements
    -- The functional arg has type Maybe b
    -- if we have Nothing, no elem added to result, if Just b, then added
    --- flip evaluates the function flipping the order of arguments

      in case pols of
        [] -> Nothing
        p -> if all (== Pos) p
             then Just Pos
             else if all (== Neg) p
                  then Just Neg
                  else Just Mix

litElim :: Expr -> Expr
litElim ex =
  let lits = Set.toList (getLiterals ex)
      pols = map (getPolarity ex) ls

  -- Assign values
  polAssign :: Char -> Maybe Polarity -> Maybe (Char, Bool)
  polAssign v (Just Pos) = Just (v, True)
  polAssign v (Just Neg) = Just (v, False)
