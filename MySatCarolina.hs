import CNF
--import CNF.DIMACS

--import qualified Solver.Naive as Naive

import Data.List
import Control.Monad
import System.Environment
import System.Exit
import GHC.Maybe (Maybe(Nothing))

opposite (Lit var pol) = Lit var (not pol)

condition :: Lit -> [Cls] -> [Cls]
-- conditions a list of clauses by a literal
-- basically gives you F | l
condition x bnf = map removeXbar $ filter hasNoX bnf
  where hasNoX cls = not $ x `elem` literals cls
        removeXbar cls = BigOr $ filter (\a -> a /= xbar) $ literals cls
        xbar = opposite x

solve :: [Cls] -> [Subst]
-- lazily constructs all partial satisfying assignments for a list of clauses.
solve [] = [[]]
solve (x : xs)
  | null (literals x) = []
  | not (null sol1) = [unLit lit : head sol1]
  | not (null sol2) = [unLit (opposite lit) : head sol2]
  | otherwise = []
  where
      f lit = solve (condition lit (x : xs))
      lit = head (literals x)
      sol1 = f lit
      sol2 = f $ opposite lit

-- if F has a clause with a single literal then
-- the formula can be reduced to F | l
testLen :: Cls -> Bool
testLen c = if (length c == 1) then True else False


solution :: CNF -> Maybe Subst
solution cnf
  | null res = Nothing
  | otherwise =  Just sub
  where res = solve(clauses cnf)
        sol = head res -- [(Int, Bool)]
        getsol v = case find ((==v) . fst) sol of
          Just x -> x
          otherwise -> (v, True)
        sub = map getsol (vars cnf)


main :: IO ()
main = do
  name <- getProgName
  args <- getArgs
  unless (length args == 1) $ do
    putStrLn ("Usage: " ++ name ++ " <cnf file>")
    exitFailure
  f <- readCNFfromDIMACS (head args)
  -- case Naive.solution f of
  case solution f of
    Nothing  -> putStrLn "UNSAT"
    Just rho -> putStrLn ("SAT\n" ++ dimacsSubst rho)
