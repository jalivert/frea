module Compiler.TypeAnalyzer.AnalyzeState where


import qualified Data.Map.Strict as Map


-- | Rigid (skolem) type variables introduced by type annotations.
-- Maps the internal skolem name to the user-written name for error messages.
type RigidVars = Map.Map String String


-- Inference state
data AnalyzeState
  = AnalizeState { count :: Int, rigid'vars :: RigidVars }


-- initial inference state
init'analyze :: AnalyzeState
init'analyze = AnalizeState { count = 0, rigid'vars = Map.empty }
