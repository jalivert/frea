module TypeSynonymSpec where

import Test.Hspec
import System.Exit

import qualified Data.Map.Strict as Map

import Compiler.Parser.Parser (parse'expr, parse'type)
import Compiler.Syntax.Declaration
import Compiler.Syntax.Type
import Compiler.Syntax.Expression
import Compiler.Syntax.Kind (Kind(..))

import Compiler.TypeAnalyzer.TypeOf (infer'expression, analyze'module)
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.Error
import Compiler.TypeAnalyzer.Types
import Compiler.TypeAnalyzer.Type.Evaluate (evaluate)


spec :: Spec
spec = describe "Test type synonyms and type evaluation" $ do
  describe "Type synonym parsing" $ do
    it "Parses simple type synonym" $ do
      "module Main where { type String = List Char }" <=>*
        [ TypeAlias "String" (TyApp (TyCon "List") (TyCon "Char")) ]

    it "Parses polymorphic type synonym" $ do
      -- NOTE: params fold into TyOp per Parser.y
      "module Main where { type Pair a = (a, a) }" <=>*
        [ TypeAlias "Pair" (TyOp "a" (TyTuple [TyVar "a", TyVar "a"])) ]

    it "Parses function type synonym" $ do
      "module Main where { type Predicate a = a -> Bool }" <=>*
        [ TypeAlias "Predicate" (TyOp "a" (TyArr (TyVar "a") (TyCon "Bool"))) ]

    it "Parses nested type synonyms" $ do
      "module Main where { type A = B ; type B = Int }" <=>*
        [ TypeAlias "A" (TyCon "B")
        , TypeAlias "B" (TyCon "Int")
        ]

  describe "Type synonym expansion" $ do
    let env = empty't'env
        aliEnv = Map.fromList
          [ ("String", TyApp (TyCon "List") (TyCon "Char"))
          -- NOTE: parameterized synonyms are TyOps (see parser); the test
          -- setup previously used bare tuples which cannot beta-reduce.
          , ("Pair", TyOp "a" (TyTuple [TyVar "a", TyVar "a"]))
          , ("Predicate", TyOp "a" (TyArr (TyVar "a") (TyCon "Bool")))
          , ("A", TyCon "B")
          , ("B", TyCon "Int")
          ]
        aEnv = AEnv empty'k'env env aliEnv

    it "Expands String to List Char" $ do
      let ty = parse'type "String"
      run'analyze aEnv (evaluate ty) `shouldBe` Right (TyApp (TyCon "List") (TyCon "Char"))

    it "Expands nested synonyms A -> B -> Int" $ do
      let ty = parse'type "A"
      run'analyze aEnv (evaluate ty) `shouldBe` Right (TyCon "Int")

    it "Expands Pair Int to (Int, Int)" $ do
      let ty = parse'type "Pair Int"
      run'analyze aEnv (evaluate ty) `shouldBe` Right (TyTuple [TyCon "Int", TyCon "Int"])

    it "Expands Predicate Int to Int -> Bool" $ do
      let ty = parse'type "Predicate Int"
      run'analyze aEnv (evaluate ty) `shouldBe` Right (TyArr (TyCon "Int") (TyCon "Bool"))

  describe "Type inference with synonyms" $ do
    let kEnv = empty'k'env `Map.union` Map.fromList
          [ ("Bool", Star)
          , ("List", KArr Star Star)
          , ("Char", Star)
          , ("Int", Star)
          ]
        -- NOTE: tests need value constructors in scope; empty't'env only has
        -- `#`-prims, so provide list/enum basics explicitly.
        tEnv = empty't'env `Map.union` Map.fromList
          [ ("[]", ForAll ["a"] (TyApp (TyCon "List") (TyVar "a")))
          , (":", ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyApp (TyCon "List") (TyVar "a")))))
          , ("+", ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyVar "a") (TyVar "a"))))
          , ("which-List", ForAll ["a", "r"] (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyArr (TyVar "r") (TyArr (TyArr (TyVar "a") (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyVar "r"))) (TyVar "r")))))
          ]
        aliEnv = Map.fromList
          [ ("String", TyApp (TyCon "List") (TyCon "Char"))
          , ("Nat", TyCon "Int")
          ]
        aEnv = AEnv kEnv tEnv aliEnv

    it "Infers type of String literal" $ do
      "\"hello\"" <::> const (ForAll [] (TyApp (TyCon "List") (TyCon "Char")))

    it "Infers type of annotated String" $ do
      "(\"hello\" :: String)" <::> const (ForAll [] (TyApp (TyCon "List") (TyCon "Char")))

    it "Infers type of function using String" $ do
      -- NOTE: frea infix is n-ary; parenthesize the applied arg.
      "let { len = \\ s -> which-List s 0 (\\ h t -> 1 + (len t)) } in len" <::> const (ForAll ["a"] (TyApp (TyCon "List") (TyVar "a") `TyArr` TyCon "Int"))

  describe "Type synonym cycles" $ do
    it "Detects direct cycle" $ do
      let aliEnv = Map.fromList [("A", TyCon "A")]
          aEnv = AEnv empty'k'env empty't'env aliEnv
          decls = [TypeAlias "A" (TyCon "A")]
      -- analyze'module should fail with cycle detection
      -- This test requires the module analysis to be run
      pending

    it "Detects indirect cycle" $ do
      let aliEnv = Map.fromList [("A", TyCon "B"), ("B", TyCon "A")]
          aEnv = AEnv empty'k'env empty't'env aliEnv
      pending

    it "Detects cycle through type application" $ do
      let aliEnv = Map.fromList [("Foo", TyApp (TyCon "Bar") (TyCon "Int")), ("Bar", TyApp (TyCon "Foo") (TyCon "Bool"))]
          aEnv = AEnv empty'k'env empty't'env aliEnv
      pending

infix 4 <=>*

(<=>*) :: String -> [Declaration] -> IO ()
(<=>*) expr reference = do
  case parse'expr expr of
    Left decls -> map show decls `shouldBe` map show reference
    Right ast' -> exitFailure

infix 4 <::>

(<::>) :: String -> (AnalyzeEnv -> Scheme) -> IO ()
(<::>) expr schemeFn = do
  case parse'expr expr of
    Left cmd -> exitFailure
    Right ast -> do
      let kEnv = empty'k'env `Map.union` Map.fromList
            [ ("Bool", Star)
            , ("List", KArr Star Star)
            , ("Char", Star)
            , ("Int", Star)
            ]
          tEnv = empty't'env `Map.union` Map.fromList
            [ ("[]", ForAll ["a"] (TyApp (TyCon "List") (TyVar "a")))
            , (":", ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyApp (TyCon "List") (TyVar "a")))))
            , ("+", ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyVar "a") (TyVar "a"))))
            , ("which-List", ForAll ["a", "r"] (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyArr (TyVar "r") (TyArr (TyArr (TyVar "a") (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyVar "r"))) (TyVar "r")))))
            ]
          aliEnv = Map.fromList
            [ ("String", TyApp (TyCon "List") (TyCon "Char"))
            , ("Nat", TyCon "Int")
            ]
          aEnv = AEnv kEnv tEnv aliEnv
      (run'analyze aEnv (infer'expression ast))
        `shouldBe` Right (schemeFn aEnv)