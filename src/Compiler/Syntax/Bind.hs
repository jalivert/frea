module Compiler.Syntax.Bind where

import Compiler.Syntax.MatchGroup (MatchGroup, Match)
import Compiler.Syntax.Signature (Sig)

data Bind
  = FunBind String Match