-- Getting import errors so I made a new file
import Data.List
import Control.Monad
import System.Environment
import System.Exit
import GHC.Maybe (Maybe(Nothing))

type Var = Int
data Lit = Lit    { var :: Var , pol :: Bool }                 deriving (Ord,Show,Eq)
data Cls = BigOr  { literals :: [Lit] }                        deriving (Show,Eq)
data CNF = BigAnd { vars :: [Var], clauses  :: [Cls] }         deriving (Show,Eq)

type Subst = [(Var,Bool)]

numClss :: CNF -> Int
numClss = length . clauses

numLits :: CNF -> Int
numLits = sum . map (length . literals) . clauses

checkLen :: Cls -> Bool
checkLen cls = (n == 1)
          where n = length (literals cls)

unitProp :: CNF -> [Cls]
unitProp cnf = filter (checkLen) (clauses cnf)
