import CNF
import CNF.DIMACS

import qualified Solver.Naive as Naive

import Control.Monad
import System.Environment
import System.Exit

main :: IO ()
main = do
  name <- getProgName
  args <- getArgs
  unless (length args == 1) $ do
    putStrLn ("Usage: " ++ name ++ " <cnf file>")
    exitFailure
  f <- readCNFfromDIMACS (args !! 0)
  case Naive.solution f of
    Nothing  -> putStrLn "UNSAT"
    Just rho -> putStrLn ("SAT\n" ++ dimacsSubst rho)
