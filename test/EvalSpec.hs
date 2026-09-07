module EvalSpec where

import Test.Hspec
import System.Exit
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map

import Compiler.Parser.Parser (parse'expr)
import Interpreter.Value (Value(..), Env, Memory, EvaluationError(..))
import Interpreter.Address (Address(Addr))
import Interpreter.Evaluate
import Compiler.Syntax.Literal
import Compiler.Syntax.Expression (Expression)
import qualified Compiler.Syntax.Expression as E
import Compiler.Syntax.Type (Type(..))
import Compiler.Syntax.Declaration (ConstrDecl(..))
import Compiler.TypeAnalyzer.TypeOf (register'constr'insts, register'elim'insts, generate'constr'insts, generate'elim'insts)


spec :: Spec
spec = describe "Test evaluation of expressions" $ do
  describe "Literals" $ do
    it "Evaluates simple integer constant" $ do
      "1" `evals'to` Lit (LitInt 1)

    it "Evaluates negative integer" $ do
      "-1" `evals'to` Lit (LitInt (-1))

    it "Evaluates double constant" $ do
      "1.5" `evals'to` Lit (LitDouble 1.5)

    it "Evaluates char constant" $ do
      "'c'" `evals'to` Lit (LitChar 'c')

    it "Evaluates string constant" $ do
      "\"hello\"" `evals'to` Data ":" [ Lit (LitChar 'h')
                                      , Data ":" [ Lit (LitChar 'e')
                                                 , Data ":" [ Lit (LitChar 'l')
                                                            , Data ":" [ Lit (LitChar 'l')
                                                                       , Data ":" [ Lit (LitChar 'o')
                                                                                    , Data "[]" [] ]]]]]

    it "Evaluates unit" $ do
      "()" `evals'to` Data "()" []

    it "Evaluates True" $ do
      "True" `evals'to` Data "True" []

    it "Evaluates False" $ do
      "False" `evals'to` Data "False" []

  describe "Lambda application" $ do
    it "Evaluates simple lambda application" $ do
      "((lambda x -> x) 23)" `evals'to` Lit (LitInt 23)

    it "Evaluates lambda with multiple arguments" $ do
      "((lambda x y -> x) 1 2)" `evals'to` Lit (LitInt 1)

    it "Evaluates curried lambda" $ do
      "let { add = lambda x y -> x + y } in add 1 2" `evals'to` Lit (LitInt 3)

    it "Evaluates lambda returning lambda" $ do
      "let { const = lambda x y -> x } in (const 1) 2" `evals'to` Lit (LitInt 1)

  describe "Let expressions" $ do
    it "Evaluates simple let" $ do
      "let { x = 1 } in x" `evals'to` Lit (LitInt 1)

    it "Evaluates let with multiple bindings" $ do
      "let { x = 1 ; y = 2 } in x + y" `evals'to` Lit (LitInt 3)

    it "Evaluates let with shadowing" $ do
      "let { x = 1 ; x = 2 } in x" `evals'to` Lit (LitInt 2)

    it "Evaluates nested let" $ do
      "let { x = 1 } in let { y = x + 1 } in y" `evals'to` Lit (LitInt 2)

    it "Evaluates let with function" $ do
      "let { f = lambda x -> x + 1 } in f 5" `evals'to` Lit (LitInt 6)

  describe "Conditionals" $ do
    it "Evaluates if-then-else (true branch)" $ do
      "if True then 1 else 2" `evals'to` Lit (LitInt 1)

    it "Evaluates if-then-else (false branch)" $ do
      "if False then 1 else 2" `evals'to` Lit (LitInt 2)

    it "Evaluates nested if" $ do
      "if True then if False then 1 else 2 else 3" `evals'to` Lit (LitInt 2)

    it "Evaluates if with complex condition" $ do
      "if 1 < 2 then 10 else 20" `evals'to` Lit (LitInt 10)

  describe "Primitive operations" $ do
    it "Evaluates integer addition" $ do
      "(#+ (1, 2))" `evals'to` Lit (LitInt 3)

    it "Evaluates integer subtraction" $ do
      "(#- (5, 3))" `evals'to` Lit (LitInt 2)

    it "Evaluates integer multiplication" $ do
      "(#* (4, 5))" `evals'to` Lit (LitInt 20)

    it "Evaluates integer division" $ do
      "(#div (10, 2))" `evals'to` Lit (LitInt 5)

    it "Evaluates double addition" $ do
      "(#+. (1.5, 2.5))" `evals'to` Lit (LitDouble 4.0)

    it "Evaluates double subtraction" $ do
      "(#-. (5.5, 2.0))" `evals'to` Lit (LitDouble 3.5)

    it "Evaluates double multiplication" $ do
      "(#*. (2.5, 4.0))" `evals'to` Lit (LitDouble 10.0)

    it "Evaluates double division" $ do
      "(#/ (10.0, 2.0))" `evals'to` Lit (LitDouble 5.0)

    it "Evaluates equality (true)" $ do
      "(#= (1, 1))" `evals'to` Data "True" []

    it "Evaluates equality (false)" $ do
      "(#= (1, 2))" `evals'to` Data "False" []

    it "Evaluates less than (true)" $ do
      "(#< (1, 2))" `evals'to` Data "True" []

    it "Evaluates less than (false)" $ do
      "(#< (2, 1))" `evals'to` Data "False" []

    it "Evaluates greater than (true)" $ do
      "(#> (2, 1))" `evals'to` Data "True" []

    it "Evaluates fst" $ do
      "(#fst (1, 2))" `evals'to` Lit (LitInt 1)

    it "Evaluates snd" $ do
      "(#snd (1, 2))" `evals'to` Lit (LitInt 2)

    it "Evaluates show on int" $ do
      "(#show 42)" `evals'to` Data ":" [Lit (LitChar '4'), Data ":" [Lit (LitChar '2'), Data "[]" []]]

    it "Evaluates show on double" $ do
      "(#show 3.14)" `evals'to` Data ":" [Lit (LitChar '3'), Data ":" [Lit (LitChar '.'), Data ":" [Lit (LitChar '1'), Data ":" [Lit (LitChar '4'), Data "[]" []]]]]

    it "Evaluates show on char" $ do
      -- NOTE: `#show` uses Haskell's `show` on the value, so a Char renders
      -- with surrounding single quotes: "'a'" is a 3-char string.
      "(#show 'a')" `evals'to` Data ":" [Lit (LitChar '\''), Data ":" [Lit (LitChar 'a'), Data ":" [Lit (LitChar '\''), Data "[]" []]]]

    it "Evaluates show on bool" $ do
      "(#show True)" `evals'to` Data ":" [Lit (LitChar 'T'), Data ":" [Lit (LitChar 'r'), Data ":" [Lit (LitChar 'u'), Data ":" [Lit (LitChar 'e'), Data "[]" []]]]]

  describe "Tuples" $ do
    it "Evaluates tuple" $ do
      "(1, 2)" `evals'to` Tuple [Lit (LitInt 1), Lit (LitInt 2)]

    it "Evaluates 3-tuple" $ do
      "(1, 2, 3)" `evals'to` Tuple [Lit (LitInt 1), Lit (LitInt 2), Lit (LitInt 3)]

    it "Evaluates nested tuple" $ do
      "((1, 2), 3)" `evals'to` Tuple [Tuple [Lit (LitInt 1), Lit (LitInt 2)], Lit (LitInt 3)]

  describe "Lists" $ do
    it "Evaluates empty list" $ do
      "[]" `evals'to` Data "[]" []

    it "Evaluates cons" $ do
      "1 : []" `evals'to` Data ":" [Lit (LitInt 1), Data "[]" []]

    it "Evaluates list literal" $ do
      "[1, 2, 3]" `evals'to` Data ":" [Lit (LitInt 1), Data ":" [Lit (LitInt 2), Data ":" [Lit (LitInt 3), Data "[]" []]]]

    it "Evaluates list concatenation" $ do
      "let { (++) = \\ a b -> which-List a b (\\ h t -> h : (t ++ b)) } in [1, 2] ++ [3, 4]" `evals'to` Data ":" [Lit (LitInt 1), Data ":" [Lit (LitInt 2), Data ":" [Lit (LitInt 3), Data ":" [Lit (LitInt 4), Data "[]" []]]]]

    it "Evaluates head" $ do
      "let { head = \\ lst -> which-List lst Nothing (\\ h t -> Just h) } in head [1, 2, 3]" `evals'to` Data "Just" [Lit (LitInt 1)]

    it "Evaluates tail" $ do
      "let { tail = \\ lst -> which-List lst Nothing (\\ h t -> Just t) } in tail [1, 2, 3]" `evals'to` Data "Just" [Data ":" [Lit (LitInt 2), Data ":" [Lit (LitInt 3), Data "[]" []]]]

    it "Evaluates map" $ do
      "let { map = \\ f lst -> which-List lst [] (\\ h t -> (f h) : (map f t)) ; double = \\ x -> x * 2 } in map double [1, 2, 3]" `evals'to` Data ":" [Lit (LitInt 2), Data ":" [Lit (LitInt 4), Data ":" [Lit (LitInt 6), Data "[]" []]]]

    it "Evaluates foldl (sum)" $ do
      "let { foldl = \\ f acc lst -> which-List lst acc (\\ h t -> foldl f (f acc h) t) ; sum = foldl (\\ a b -> a + b) 0 } in sum [1, 2, 3, 4, 5]" `evals'to` Lit (LitInt 15)

  describe "Recursive functions" $ do
    it "Evaluates factorial" $ do
      -- NOTE: frea infix is n-ary, so applied args need parens.
      "let { fact = \\ n -> if n == 0 then 1 else n * (fact (n - 1)) } in fact 5" `evals'to` Lit (LitInt 120)

    it "Evaluates fibonacci" $ do
      "let { fib = \\ n -> if n < 2 then n else (fib (n - 1)) + (fib (n - 2)) } in fib 10" `evals'to` Lit (LitInt 55)

    it "Evaluates list length" $ do
      "let { length = \\ lst -> which-List lst 0 (\\ h t -> 1 + (length t)) } in length [1, 2, 3, 4, 5]" `evals'to` Lit (LitInt 5)

    it "Evaluates list reverse" $ do
      "let { reverse = \\ lst -> which-List lst [] (\\ h t -> (reverse t) ++ [h]) } in reverse [1, 2, 3]" `evals'to` Data ":" [Lit (LitInt 3), Data ":" [Lit (LitInt 2), Data ":" [Lit (LitInt 1), Data "[]" []]]]

    it "Evaluates take" $ do
      "let { take = \\ n lst -> if n == 0 then [] else which-List lst [] (\\ h t -> h : (take (n - 1) t)) } in take 3 [1, 2, 3, 4, 5]" `evals'to` Data ":" [Lit (LitInt 1), Data ":" [Lit (LitInt 2), Data ":" [Lit (LitInt 3), Data "[]" []]]]

  describe "Data types and eliminators" $ do
    it "Evaluates Maybe Just" $ do
      "Just 42" `evals'to` Data "Just" [Lit (LitInt 42)]

    it "Evaluates Maybe Nothing" $ do
      "Nothing" `evals'to` Data "Nothing" []

    it "Evaluates which-Maybe on Just" $ do
      "which-Maybe (Just 42) 0 (\\ x -> x + 1)" `evals'to` Lit (LitInt 43)

    it "Evaluates which-Maybe on Nothing" $ do
      "which-Maybe Nothing 0 (\\ x -> x + 1)" `evals'to` Lit (LitInt 0)

    it "Evaluates Either Left" $ do
      "Left 42" `evals'to` Data "Left" [Lit (LitInt 42)]

    it "Evaluates Either Right" $ do
      "Right 42" `evals'to` Data "Right" [Lit (LitInt 42)]

    it "Evaluates which-Either on Left" $ do
      "which-Either (Left 42) (\\ x -> x + 1) (\\ y -> y * 2)" `evals'to` Lit (LitInt 43)

    it "Evaluates which-Either on Right" $ do
      "which-Either (Right 42) (\\ x -> x + 1) (\\ y -> y * 2)" `evals'to` Lit (LitInt 84)

  describe "Lazy evaluation" $ do
    it "Does not evaluate unused branch" $ do
      "let { x = #debug 1 ; y = #debug 2 } in if True then x else y" `evals'to` Lit (LitInt 1)

    it "Shares evaluated thunks" $ do
      "let { x = #debug 42 ; y = (x, x) } in #fst y" `evals'to` Lit (LitInt 42)

    it "Handles infinite lists" $ do
      "let { ones = 1 : ones ; take = \\ n lst -> if n == 0 then [] else which-List lst [] (\\ h t -> h : (take (n - 1) t)) } in take 5 ones" `evals'to` Data ":" [Lit (LitInt 1), Data ":" [Lit (LitInt 1), Data ":" [Lit (LitInt 1), Data ":" [Lit (LitInt 1), Data ":" [Lit (LitInt 1), Data "[]" []]]]]]

  describe "Fix point" $ do
    it "fix not fully implemented" pending
    -- it "Evaluates fix factorial" $ do
    --   "fix (\\ fact n -> if n == 0 then 1 else n * fact (n - 1)) 5" `evals'to` Lit (LitInt 120)

    -- it "Evaluates fix fibonacci" $ do
    --   "fix (\\ fib n -> if n < 2 then n else fib (n - 1) + fib (n - 2)) 10" `evals'to` Lit (LitInt 55)

  describe "Operators" $ do
    it "Evaluates prefix operator" $ do
      "(+) 1 2" `evals'to` Lit (LitInt 3)

    it "Evaluates infix operator" $ do
      "1 + 2" `evals'to` Lit (LitInt 3)

    it "Evaluates backtick operator" $ do
      -- NOTE: bare `` `plus` `` is not a valid binder; use infix binding form.
      "let { a `plus` b = (#+ (a, b)) } in 1 `plus` 2" `evals'to` Lit (LitInt 3)

  describe "Error cases" $ do
    it "error handling not fully implemented" pending
    -- These would need proper error handling in the evaluator
    -- it "Handles division by zero" $ do
    --   "(#div (1, 0))" `evalShouldFail` DivisionByZero 1

    -- it "Handles unbound variable" $ do
    --   "x" `evalShouldFail` UnboundVar "x"


-- NOTE: the old minimal testEnv (bare `Data ":" []` etc.) could not apply
-- constructors as functions and had no eliminators/operators in scope, so
-- every test using `which-*`, `Just`, `+`, `==`, ... failed with
-- `Unknown variable`. Build a small prelude env the same way Main does
-- (constructor lambdas + eliminators), plus sugar operators defined via
-- `#`-prims (which need no env lookup).
testEnv :: Env
testEnv = fullEnv

testMemory :: Memory
testMemory = fullMemory

fullEnv :: Env
fullEnv = envWithOps
  where
    decls =
      [ ("Bool", [] :: [String], [ConDecl "True" [], ConDecl "False" []])
      , ("List", ["a"], [ConDecl "[]" [], ConDecl ":" [TyVar "a", TyApp (TyCon "List") (TyVar "a")]])
      , ("Maybe", ["a"], [ConDecl "Nothing" [], ConDecl "Just" [TyVar "a"]])
      , ("Either", ["a", "b"], [ConDecl "Left" [TyVar "a"], ConDecl "Right" [TyVar "b"]])
      , ("()", [], [ConDecl "()" []])
      ]
    envWithConstrs = foldl (\ env (_, _, constrs) -> register'constr'insts constrs env) Map.empty decls
    envWithElims = foldl (\ env (name, _, constrs) -> register'elim'insts name constrs env) envWithConstrs decls
    -- sugar operators; each gets a fresh address after the data decls
    opDefs =
      [ ("++", E.Lam "la" (E.Lam "lb" (E.App (E.App (E.App (E.Var "which-List") (E.Var "la")) (E.Var "lb")) (E.Lam "ha" (E.Lam "ta" (E.App (E.App (E.Var ":") (E.Var "ha")) (E.App (E.App (E.Var "++") (E.Var "ta")) (E.Var "lb"))))))))
      , ("+", E.Lam "a" (E.Lam "b" (E.App (E.Op "#+") (E.Tuple [E.Var "a", E.Var "b"]))))
      , ("-", E.Lam "a" (E.Lam "b" (E.App (E.Op "#-") (E.Tuple [E.Var "a", E.Var "b"]))))
      , ("*", E.Lam "a" (E.Lam "b" (E.App (E.Op "#*") (E.Tuple [E.Var "a", E.Var "b"]))))
      , ("==", E.Lam "a" (E.Lam "b" (E.App (E.Op "#=") (E.Tuple [E.Var "a", E.Var "b"]))))
      , ("<", E.Lam "a" (E.Lam "b" (E.App (E.Op "#<") (E.Tuple [E.Var "a", E.Var "b"]))))
      , (">", E.Lam "a" (E.Lam "b" (E.App (E.Op "#>") (E.Tuple [E.Var "a", E.Var "b"]))))
      ]
    envWithOps = foldl (\ env (name, _) -> Map.insert name (Addr (Map.size env)) env) envWithElims opDefs

fullMemory :: Memory
fullMemory = memWithOps
  where
    decls =
      [ ("Bool", [] :: [String], [ConDecl "True" [], ConDecl "False" []])
      , ("List", ["a"], [ConDecl "[]" [], ConDecl ":" [TyVar "a", TyApp (TyCon "List") (TyVar "a")]])
      , ("Maybe", ["a"], [ConDecl "Nothing" [], ConDecl "Just" [TyVar "a"]])
      , ("Either", ["a", "b"], [ConDecl "Left" [TyVar "a"], ConDecl "Right" [TyVar "b"]])
      , ("()", [], [ConDecl "()" []])
      ]
    memWithConstrs = foldl (\ mem (_, _, constrs) -> generate'constr'insts constrs fullEnv mem) Map.empty decls
    memWithElims = foldl (\ mem (name, _, constrs) -> generate'elim'insts name constrs fullEnv mem) memWithConstrs decls
    opDefs =
      [ ("++", E.Lam "la" (E.Lam "lb" (E.App (E.App (E.App (E.Var "which-List") (E.Var "la")) (E.Var "lb")) (E.Lam "ha" (E.Lam "ta" (E.App (E.App (E.Var ":") (E.Var "ha")) (E.App (E.App (E.Var "++") (E.Var "ta")) (E.Var "lb"))))))))
      , ("+", E.Lam "a" (E.Lam "b" (E.App (E.Op "#+") (E.Tuple [E.Var "a", E.Var "b"]))))
      , ("-", E.Lam "a" (E.Lam "b" (E.App (E.Op "#-") (E.Tuple [E.Var "a", E.Var "b"]))))
      , ("*", E.Lam "a" (E.Lam "b" (E.App (E.Op "#*") (E.Tuple [E.Var "a", E.Var "b"]))))
      , ("==", E.Lam "a" (E.Lam "b" (E.App (E.Op "#=") (E.Tuple [E.Var "a", E.Var "b"]))))
      , ("<", E.Lam "a" (E.Lam "b" (E.App (E.Op "#<") (E.Tuple [E.Var "a", E.Var "b"]))))
      , (">", E.Lam "a" (E.Lam "b" (E.App (E.Op "#>") (E.Tuple [E.Var "a", E.Var "b"]))))
      ]
    memWithOps = foldl (\ mem (name, expr) -> Map.insert (fullEnv Map.! name) (Thunk (\ env -> force expr env) fullEnv (fullEnv Map.! name)) mem) memWithElims opDefs


-- | Deep-force thunks inside tuples/data so `show` comparisons don't see
-- `<thunk>`. The interpreter is lazy by design; `force` alone leaves
-- `Tuple [Thunk, ...]`. The REPL uses `Print.print` for this reason.
deepForce :: Value -> State Memory (Either EvaluationError Value)
deepForce (Thunk f env addr) = do
  r <- force'val (Thunk f env addr)
  case r of
    Left err -> return (Left err)
    Right v -> deepForce v
deepForce (Tuple vals) = do
  rs <- mapM deepForce vals
  return (sequence rs >>= Right . Tuple)
deepForce (Data name args) = do
  rs <- mapM deepForce args
  return (sequence rs >>= Right . Data name)
deepForce v = return (Right v)

evals'to :: String -> Value -> IO ()
expr `evals'to` val = do
  case parse'expr expr of
    Left _ -> exitFailure
    Right expr -> do
      let (result, mem') = runState (force expr testEnv) testMemory
      case result of
        Left err -> expectationFailure $ "Evaluation error: " ++ show err
        Right expr' -> do
          let (deep, _) = runState (deepForce expr') mem'
          case deep of
            Left err -> expectationFailure $ "Deep-force error: " ++ show err
            Right v -> show v `shouldBe` show val