module InferenceSpec where

import Test.Hspec
import System.Exit

import qualified Data.Map.Strict as Map

import Compiler.Parser.Parser (parse'expr)

import Compiler.Syntax.Expression
import Compiler.Syntax.Type
import Compiler.Syntax.Literal


import Compiler.TypeAnalyzer.TypeOf (infer'expression)
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.Error

-- Type aliases for common types
t'Int :: Type
t'Int = TyCon "Int"

t'Double :: Type
t'Double = TyCon "Double"

t'Char :: Type
t'Char = TyCon "Char"

t'Bool :: Type
t'Bool = TyCon "Bool"


env :: TypeEnv
env = empty't'env `Map.union` Map.fromList
  [ ("True"   , ForAll [] (TyCon "Bool"))
  , ("False"  , ForAll [] (TyCon "Bool"))
  , ("()"     , ForAll [] (TyCon "()"))
  , ("[]"     , ForAll ["a"] (TyApp (TyCon "List") (TyVar "a")))
  , (":"      , ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyApp (TyCon "List") (TyVar "a")))))
  , ("+"      , ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyVar "a") (TyVar "a"))))
  , ("*"      , ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyVar "a") (TyVar "a"))))
  , ("-"      , ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyVar "a") (TyVar "a"))))
  , ("=="     , ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyVar "a") t'Bool)))
  , ("<"      , ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyVar "a") t'Bool)))
  , (">", ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyVar "a") t'Bool)))
  , ("Nothing", ForAll ["a"] (TyApp (TyCon "Maybe") (TyVar "a")))
  , ("Just"   , ForAll ["a"] (TyArr (TyVar "a") (TyApp (TyCon "Maybe") (TyVar "a"))))
  , ("#+"     , ForAll [] (TyArr (TyTuple [t'Int, t'Int]) t'Int))
  , ("#+."    , ForAll [] (TyArr (TyTuple [t'Double, t'Double]) t'Double))
  , ("#-"     , ForAll [] (TyArr (TyTuple [t'Int, t'Int]) t'Int))
  , ("#-."    , ForAll [] (TyArr (TyTuple [t'Double, t'Double]) t'Double))
  , ("#*"     , ForAll [] (TyArr (TyTuple [t'Int, t'Int]) t'Int))
  , ("#*."    , ForAll [] (TyArr (TyTuple [t'Double, t'Double]) t'Double))
  , ("#div"   , ForAll [] (TyArr (TyTuple [t'Int, t'Int]) t'Int))
  , ("#/"     , ForAll [] (TyArr (TyTuple [t'Double, t'Double]) t'Double))
  , ("#="     , ForAll ["a"] (TyArr (TyTuple [TyVar "a", TyVar "a"]) t'Bool))
  , ("#<"     , ForAll ["a"] (TyArr (TyTuple [TyVar "a", TyVar "a"]) t'Bool))
  , ("#>"     , ForAll ["a"] (TyArr (TyTuple [TyVar "a", TyVar "a"]) t'Bool))
  , ("#fst"   , ForAll ["a", "b"] (TyArr (TyTuple [TyVar "a", TyVar "b"]) (TyVar "a")))
  , ("#snd"   , ForAll ["a", "b"] (TyArr (TyTuple [TyVar "a", TyVar "b"]) (TyVar "b")))
  , ("#show"  , ForAll ["a"] (TyArr (TyVar "a") (TyApp (TyCon "List") (TyCon "Char"))))
  , ("#debug" , ForAll ["a"] (TyArr (TyVar "a") (TyVar "a")))
  , ("which-Bool", ForAll ["r"] (TyArr (TyCon "Bool") (TyArr (TyVar "r") (TyArr (TyVar "r") (TyVar "r")))))
  , ("which-List", ForAll ["a", "r"] (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyArr (TyVar "r") (TyArr (TyArr (TyVar "a") (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyVar "r"))) (TyVar "r")))))
  , ("which-Maybe", ForAll ["a", "r"] (TyArr (TyApp (TyCon "Maybe") (TyVar "a")) (TyArr (TyVar "r") (TyArr (TyArr (TyVar "a") (TyVar "r")) (TyVar "r")))))
  , ("which-Either", ForAll ["a", "b", "r"] (TyArr (TyApp (TyApp (TyCon "Either") (TyVar "a")) (TyVar "b")) (TyArr (TyArr (TyVar "a") (TyVar "r")) (TyArr (TyArr (TyVar "b") (TyVar "r")) (TyVar "r")))))
  , ("fix", ForAll ["a"] (TyArr (TyArr (TyVar "a") (TyVar "a")) (TyVar "a")))
  ]


spec :: Spec
spec = describe "Test the inference" $ do
  describe "Literals" $ do
    it "Infers the type of a single integer" $
      type'of (Lit (LitInt 23)) `shouldBe` Right (ForAll [] t'Int)

    it "Infers the type of a single double" $
      type'of (Lit (LitDouble 23.5)) `shouldBe` Right (ForAll [] t'Double)

    it "Infers the type of a single char" $
      type'of (Lit (LitChar 'c')) `shouldBe` Right (ForAll [] t'Char)

  describe "Variables and constructors" $ do
    it "Infers the type of True" $
      "True" <::> ForAll [] t'Bool

    it "Infers the type of False" $
      "False" <::> ForAll [] t'Bool

    it "Infers the type of unit" $
      "()" <::> ForAll [] (TyCon "()")

    it "Infers the type of empty list" $
      "[]" <::> ForAll ["a"] (TyApp (TyCon "List") (TyVar "a"))

  describe "Lists" $ do
    it "Infer the type of a singleton List" $
      "[23]" <::> ForAll [] (TyApp (TyCon "List") t'Int)

    it "Infers the type of a list of integers" $
      "[1, 2, 3]" <::> ForAll [] (TyApp (TyCon "List") t'Int)

    it "Infers the type of a list of lists" $
      "[[1], [2]]" <::> ForAll [] (TyApp (TyCon "List") (TyApp (TyCon "List") t'Int))

  describe "Tuples" $ do
    it "Infers the type of a tuple" $
      "(23, True, 'a')" <::> (ForAll [] $ TyTuple [t'Int, t'Bool, t'Char])

    it "Infers the type of a pair" $
      "(1, 2)" <::> ForAll [] (TyTuple [t'Int, t'Int])

    it "Infers the type of a nested tuple" $
      "((1, 2), 3)" <::> ForAll [] (TyTuple [TyTuple [t'Int, t'Int], t'Int])

  describe "Functions" $ do
    it "Infers the type of identity" $
      "\\ x -> x" <::> ForAll ["a"] (TyVar "a" `TyArr` TyVar "a")

    it "Infers the type of const" $
      "\\ x y -> x" <::> ForAll ["a", "b"] (TyArr (TyVar "a") (TyArr (TyVar "b") (TyVar "a")))

    it "Infers the type of a lambda with multiple args" $
      "\\ a b c -> c" <::> ForAll ["a", "b", "c"] (TyArr (TyVar "a") (TyArr (TyVar "b") (TyArr (TyVar "c") (TyVar "c"))))

    it "Infers the type of a let inside lambda" $
      "\\ x -> let { y = ((#+) (x, 1)) } in y" <::> ForAll [] (TyArr t'Int t'Int)

    it "Infers the type of a let inside lambda [prefix] ((+) x 1)" $
      "\\ x -> let { (+) = \\ a b -> ((#+) (a, b)) ; y = ((+) x 1) } in y" <::> ForAll [] (TyArr t'Int t'Int)

    it "Infers the type of a let inside lambda [infix] (x + 1)" $
      "\\ x -> let { (+) = (\\ a b -> ((#+) (a, b))) ; y = (x + 1) } in y" <::> ForAll [] (TyArr t'Int t'Int)

  describe "Conditionals" $ do
    it "Infers the type of an equality check (on Int) inside the lambda" $
      "(\\ x -> if ((#=) (x, 23)) then True else False)" <::> ForAll [] (TyArr t'Int t'Bool)

    it "Infers the type of a polymorphic equality check inside the lambda" $
      "(\\ x y -> ((#=) (x, y)))" <::> ForAll ["a"] (TyVar "a" `TyArr` (TyVar "a" `TyArr` t'Bool))

    it "Infers the type of if-then-else with integers" $
      "if True then 1 else 2" <::> ForAll [] t'Int

    it "Infers the type of nested if" $
      "if True then if False then 1 else 2 else 3" <::> ForAll [] t'Int

  describe "Polymorphism" $ do
    it "Infers the type of a polymorphic tuple" $
      "(\\ x -> (1, x))" <::> ForAll ["a"] (TyVar "a" `TyArr` TyTuple [t'Int, TyVar "a"])

    it "Infers the type of a list of applications" $
     "let { fn = (lambda i -> i) } in [fn 23, fn (#+ (23, 1)), fn 42]" <::> ForAll [] (TyApp (TyCon "List") t'Int)

    it "Infers the type of polymorphic function composition" $
      "let { compose = \\ f g x -> f (g x) } in compose" <::> ForAll ["a", "b", "c"] (TyArr (TyArr (TyVar "b") (TyVar "c")) (TyArr (TyArr (TyVar "a") (TyVar "b")) (TyArr (TyVar "a") (TyVar "c"))))

  describe "Let bindings" $ do
    it "Infers the type of a let with infix function expression" $
      "let { plus = (\\ a b -> ((#+) (a, b))) } in (23 `plus` 42)" <::> ForAll [] t'Int

    it "Infers the type of a let with multiple bindings" $
      "let { x = 1 ; y = 2 ; z = x + y } in z" <::> ForAll [] t'Int

    it "Infers the type of a let with polymorphic function" $
      "let { id = \\ x -> x } in (id 1, id True)" <::> ForAll [] (TyTuple [t'Int, t'Bool])

    it "Infers the type of a let with shadowing" $
      "let { x = 1 ; x = 2 } in x" <::> ForAll [] t'Int

  describe "Primitive operations" $ do
    it "Infers the type of integer addition" $
      "(#+ (23, 42))" <::> ForAll [] t'Int

    it "Infers the type of double addition" $
      "(#+. (23.0, 42.0))" <::> ForAll [] t'Double

    it "Infers the type of integer multiplication" $
      "(#* (23, 42))" <::> ForAll [] t'Int

    it "Infers the type of equality check" $
      "(#= (23, 42))" <::> ForAll [] t'Bool

    it "Infers the type of less than" $
      "(#< (23, 42))" <::> ForAll [] t'Bool

    it "Infers the type of fst" $
      "(#fst (23, 42))" <::> ForAll [] t'Int

    it "Infers the type of snd" $
      "(#snd (23, 42))" <::> ForAll [] t'Int

    it "Infers the type of show" $
      "(#show 23)" <::> ForAll [] (TyApp (TyCon "List") (TyCon "Char"))

  describe "Eliminators" $ do
    it "Infers the type of which-Bool" $
      "which-Bool True 1 2" <::> ForAll [] t'Int

    it "Infers the type of which-List on empty list" $
      "which-List [] 0 (\\ h t -> 1)" <::> ForAll [] t'Int

    it "Infers the type of which-List on cons" $
      "which-List [1,2] 0 (\\ h t -> h)" <::> ForAll [] t'Int

    it "Infers the type of head using which-List" $
      "let { head = \\ lst -> which-List lst Nothing (\\ h t -> Just h) } in head" <::> ForAll ["a"] (TyApp (TyCon "List") (TyVar "a") `TyArr` TyApp (TyCon "Maybe") (TyVar "a"))

    it "Infers the type of tail using which-List" $
      "let { tail = \\ lst -> which-List lst Nothing (\\ h t -> Just t) } in tail" <::> ForAll ["a"] (TyApp (TyCon "List") (TyVar "a") `TyArr` TyApp (TyCon "Maybe") (TyApp (TyCon "List") (TyVar "a")))

    it "Infers the type of map" $
      "let { map = \\ f lst -> which-List lst [] (\\ h t -> (f h) : (map f t)) } in map" <::> ForAll ["a", "b"] (TyArr (TyArr (TyVar "a") (TyVar "b")) (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyApp (TyCon "List") (TyVar "b"))))

  describe "Recursive functions" $ do
    it "Infers the type of factorial" $
      "let { fact = \\ n -> if ((#=) (n, 0)) then 1 else (#*) (n, (fact ((#-) (n, 1)))) } in fact" <::> ForAll [] (t'Int `TyArr` t'Int)

    it "Infers the type of fibonacci" $
      "let { fib = \\ n -> if ((#<) (n, 2)) then n else (#+) ( (fib ((#-) (n, 1))) , (fib ((#-) (n, 2))) ) } in fib" <::> ForAll [] (t'Int `TyArr` t'Int)

    it "Infers the type of list length" $
      "let { length = \\ lst -> which-List lst 0 (\\ h t -> (#+) (1, (length t))) } in length" <::> ForAll ["a"] (TyApp (TyCon "List") (TyVar "a") `TyArr` t'Int)

    it "Infers the type of list append" $
      "let { append = \\ a b -> which-List a b (\\ h t -> h : (append t b)) } in append" <::> ForAll ["a"] (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyApp (TyCon "List") (TyVar "a"))))

  describe "Type annotations" $ do
    it "Respects explicit type annotation on expression" $
      "((\\ x -> x) :: Int -> Int)" <::> ForAll [] (TyArr t'Int t'Int)

    it "Respects explicit type annotation on let binding" $
      -- NOTE: `let { id :: ... ; ... }` is not supported in expression-lets
      -- (Annotation ';' Binding only exists at top-level Fun). Test the
      -- supported parenthesized form instead.
      "let { id = ((\\ x -> x) :: Int -> Int) } in id" <::> ForAll [] (TyArr t'Int t'Int)

  describe "Operators" $ do
    it "Infers the type of infix operator" $
      "23 + 42" <::> ForAll [] t'Int

    it "Infers the type of backtick operator" $
      -- NOTE: backtick function must be in scope; use a let-bound plus.
      "let { plus = \\ a b -> ((#+) (a, b)) } in (23 `plus` 42)" <::> ForAll [] t'Int

    it "Infers the type of prefix operator application" $
      "(+) 23 42" <::> ForAll [] t'Int

  describe "Complex expressions" $ do
    it "Infers the type of a complex let expression" $
      "let { x = 1 ; y = x + 1 ; z = y * 2 } in z" <::> ForAll [] t'Int

    it "Infers the type of nested lets" $
      "let { x = 1 } in let { y = x + 1 } in y" <::> ForAll [] t'Int

    it "Infers the type of lambda returning lambda" $
      -- NOTE: was `a `TyArr` b `TyArr` a` which parses (left-assoc) as
      -- `(a -> b) -> a`; correct type is `a -> b -> a`.
      "\\ x -> \\ y -> x" <::> ForAll ["a", "b"] (TyArr (TyVar "a") (TyArr (TyVar "b") (TyVar "a")))

infix 4 <::>

(<::>) :: String -> Scheme -> IO ()
(<::>) expr scheme =
  case parse'expr expr of
    Left cmd -> exitFailure
    Right ast ->
      (run'analyze (AEnv empty'k'env env empty'ali'env) (infer'expression ast))
        `shouldBe` Right scheme


type'of :: Expression -> Either Error Scheme
type'of expr = run'analyze (AEnv empty'k'env empty't'env empty'ali'env) (infer'expression expr)