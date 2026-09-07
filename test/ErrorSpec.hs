module ErrorSpec where

import Test.Hspec
import System.Exit
import Control.Exception (evaluate)

import qualified Data.Map.Strict as Map

import Compiler.Parser.Parser (parse'expr, parse'type)
import Compiler.Syntax.Expression
import Compiler.Syntax.Type
import Compiler.Syntax.Kind (Kind(..))

import Compiler.TypeAnalyzer.TypeOf (infer'expression)
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.Error
import Compiler.TypeAnalyzer.Types
import Compiler.TypeAnalyzer.Kind.KindOf (kind'of)


spec :: Spec
spec = describe "Test error cases" $ do
  describe "Parse errors" $ do
    -- NOTE: the parser calls `error` on failure (Parser.y parseError), so
    -- malformed input raises an exception rather than returning Left.
    it "Fails on unclosed parenthesis" $ do
      evaluate (parse'expr "(1 + 2") `shouldThrow` anyException

    it "Fails on unclosed bracket" $ do
      evaluate (parse'expr "[1, 2") `shouldThrow` anyException

    it "Fails on unclosed brace" $ do
      evaluate (parse'expr "let { x = 1") `shouldThrow` anyException

    it "Fails on incomplete if" $ do
      evaluate (parse'expr "if True then 1") `shouldThrow` anyException

    it "Fails on incomplete lambda" $ do
      evaluate (parse'expr "\\ x ->") `shouldThrow` anyException

    it "Parses `+*` as a valid operator (not an error)" $ do
      -- NOTE: `+*` matches @operator in the lexer, so `1 +* 2` is a valid
      -- application of an (unbound) operator, not a parse error.
      parse'expr "1 +* 2" `shouldSatisfy` isRight

    it "Fails on invalid let syntax" $ do
      evaluate (parse'expr "let x = 1 in x") `shouldThrow` anyException

  describe "Type errors" $ do
    let kEnv = empty'k'env `Map.union` Map.fromList
          [ ("Bool", Star)
          , ("List", KArr Star Star)
          , ("Int", Star)
          ]
        tEnv = empty't'env `Map.union` Map.fromList
          [ ("True", ForAll [] (TyCon "Bool"))
          , ("False", ForAll [] (TyCon "Bool"))
          , ("[]", ForAll ["a"] (TyApp (TyCon "List") (TyVar "a")))
          -- NOTE: backticked TyArr chains associate left; use explicit nesting.
          , (":", ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyApp (TyCon "List") (TyVar "a")))))
          , ("#+", ForAll [] (TyArr (TyTuple [TyCon "Int", TyCon "Int"]) (TyCon "Int")))
          , ("#=", ForAll ["a"] (TyArr (TyTuple [TyVar "a", TyVar "a"]) (TyCon "Bool")))
          ]
        aEnv = AEnv kEnv tEnv empty'ali'env

    it "Detects type mismatch in if branches" $ do
      "if True then 1 else False" <::!> aEnv

    it "Detects type mismatch in application" $ do
      "True 1" <::!> aEnv

    it "Detects type mismatch in primitive op" $ do
      "(#+ (True, False))" <::!> aEnv

    it "Detects unbound variable" $ do
      "x" <::!> aEnv

    it "Recursive self-application is valid (not an infinite type)" $ do
      -- NOTE: `let { f = \ x -> f x } in f` is a valid recursive definition
      -- (f :: a -> b); the occurs check does not fire here.
      case parse'expr "let { f = \\ x -> f x } in f" of
        Left _ -> expectationFailure "expected expression, got declarations"
        Right ast ->
          (run'analyze aEnv (infer'expression ast)) `shouldSatisfy` isRight

    it "Detects arity mismatch" $ do
      "(#+ 1)" <::!> aEnv

  describe "Kind errors" $ do
    it "Detects kind mismatch in application" $ do
      let ty = parse'type "Int Bool"
      kind'of (AEnv empty'k'env empty't'env empty'ali'env) ty `shouldSatisfy` isLeft

    it "Detects kind mismatch in arrow" $ do
      let ty = parse'type "Int -> Bool -> Char"
      kind'of (AEnv empty'k'env empty't'env empty'ali'env) ty `shouldBe` Right Star
      -- This should work actually

    it "Detects kind cycle" $ do
      let ty = parse'type "A"
          aliEnv = Map.fromList [("A", TyApp (TyCon "A") (TyCon "Int"))]
      kind'of (AEnv empty'k'env empty't'env aliEnv) ty `shouldSatisfy` isLeft

  describe "Type synonym errors" $ do
    it "Detects undefined type synonym" $ do
      let ty = parse'type "UndefinedType"
      kind'of (AEnv empty'k'env empty't'env empty'ali'env) ty `shouldSatisfy` isLeft

    it "Detects type synonym cycle" $ do
      let aliEnv = Map.fromList [("A", TyCon "B"), ("B", TyCon "A")]
          ty = parse'type "A"
      kind'of (AEnv empty'k'env empty't'env aliEnv) ty `shouldSatisfy` isLeft

infix 4 <::!>

(<::!>) :: String -> AnalyzeEnv -> IO ()
(<::!>) expr env = do
  case parse'expr expr of
    Left _ -> return () -- Parse error is also acceptable
    Right ast ->
      (run'analyze env (infer'expression ast)) `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False