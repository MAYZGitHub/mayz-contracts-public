module Main where
------------------------------------------------------------------------------------------
-- External Imports
------------------------------------------------------------------------------------------
-- import qualified Control.Exception  as ControlException (throwIO)
-- import qualified Data.List as DataList
-- import qualified System.Environment  as SystemEnvironment (getArgs)
import qualified Prelude                                         as P
------------------------------------------------------------------------------------------
-- Internal Imports
------------------------------------------------------------------------------------------
-- import qualified Generic.CLIHelpers as CLIHelpers
import qualified Protocol.PABSimulator as PABSimulator

------------------------------------------------------------------------------------------
-- Module
------------------------------------------------------------------------------------------
main :: P.IO ()
main = do
  -- [ nombrePool] <- SystemEnvironment.getArgs
  -- CLI.cli 
  PABSimulator.runPABSimulator
