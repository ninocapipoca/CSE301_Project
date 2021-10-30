-- types of CNF formulas and substitutions
module CNF where

import Data.List

type Var = Int
data Lit = Lit    { var :: Var , pol :: Bool }                 deriving (Ord,Show,Eq)
data Cls = BigOr  { literals :: [Lit] }                        deriving (Show,Eq)
data CNF = BigAnd { vars :: [Var], clauses  :: [Cls] }         deriving (Show,Eq)

type Subst = [(Var,Bool)]

-- destructor extracting a variable/boolean pair from a literal
unLit :: Lit -> (Var,Bool)
unLit (Lit v b) = (v,b)

-- some pretty printing routines

prettyLit :: Lit -> String
prettyLit (Lit v b) = (if b then "" else "-") ++ "x" ++ show v

prettyCls :: Cls -> String
prettyCls = intercalate " | " . map prettyLit . literals

prettyCNF :: CNF -> String
prettyCNF = intercalate " & " . map (parens . prettyCls) . clauses
  where
    parens :: String -> String
    parens s = "(" ++ s ++ ")"

-- measuring the size of formulas in terms of clauses and literals

numClss :: CNF -> Int
numClss = length . clauses

numLits :: CNF -> Int
numLits = sum . map (length . literals) . clauses

-- recursive function
solve :: [Cls] -> Maybe Subst
solve = undefined

-- conditions a list of clauses by a literal
-- emoving all clauses that contain ℓ from FFF, and eliminating the
-- negation of ℓ from remaining clauses

-- a list of clauses will be for example [(x1 OR x2 OR x3) (x3 OR x1) (x3 OR x2)]
-- or in this case BigOr [x1 x2 x3 x4]


-- no
toLits :: Cls -> [Lit]
toLits (BigOr list) = list

listOfLits :: [Cls] -> [[Lit]]
listOfLits cls = map toLits cls


conditionAux :: Lit -> [Cls] -> [Cls]
conditionAux = undefined
--conditionAux = undefined

condition :: Lit -> [Cls] -> [Cls]
condition = undefined
