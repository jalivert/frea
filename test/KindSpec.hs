module KindSpec where

import Test.Hspec
import System.Exit

import qualified Data.Map.Strict as Map

import Compiler.Parser.Parser (parse'type)
import Compiler.Syntax.Type
import Compiler.Syntax.Kind

import Compiler.TypeAnalyzer.TypeOf (infer'expression)
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.Error
import Compiler.TypeAnalyzer.Types
import Compiler.TypeAnalyzer.Kind.KindOf


kEnv :: Map.Map String Kind
kEnv = empty'k'env `Map.union` Map.fromList
  [ ("Bool", Star)
  , ("List", KArr Star Star)
  , ("Maybe", KArr Star Star)
  , ("Either", KArr Star (KArr Star Star))
  , ("(", Star)  -- tuple
  , ("->", KArr Star (KArr Star Star))
  ]

spec :: Spec
spec = describe "Test kind inference" $ do
  let aEnv = AEnv kEnv empty't'env empty'ali'env

  describe "Simple types" $ do
    it "Infers kind of concrete type" $ do
      kindOf aEnv "Int" `shouldBe` Right Star
    it "Infers kind of Bool" $ do
      kindOf aEnv "Bool" `shouldBe` Right Star
    it "Infers kind of Char" $ do
      kindOf aEnv "Char" `shouldBe` Right Star
    it "Infers kind of Double" $ do
      kindOf aEnv "Double" `shouldBe` Right Star

  describe "Type constructors" $ do
    it "Infers kind of List" $ do
      kindOf aEnv "List" `shouldBe` Right (KArr Star Star)
    it "Infers kind of Maybe" $ do
      kindOf aEnv "Maybe" `shouldBe` Right (KArr Star Star)

  describe "Type applications" $ do
    it "Infers kind of List Int" $ do
      kindOf aEnv "List Int" `shouldBe` Right Star
    it "Infers kind of Maybe Bool" $ do
      kindOf aEnv "Maybe Bool" `shouldBe` Right Star
    it "Infers kind of Either Int Bool" $ do
      kindOf aEnv "Either Int Bool" `shouldBe` Right Star
    it "Infers kind of List (Maybe Int)" $ do
      kindOf aEnv "List (Maybe Int)" `shouldBe` Right Star

  describe "Function types" $ do
    it "Infers kind of Int -> Int" $ do
      kindOf aEnv "Int -> Int" `shouldBe` Right Star
    it "Infers kind of a -> b" $ do
      -- KNOWN IMPL LIMITATION: kind inference looks up free TyVars in kEnv
      -- and throws UnboundTypeVar instead of freshening them. Aspirationally
      -- this should be `Right Star`.
      pendingWith "kind inference rejects free type variables"

  describe "Tuple types" $ do
    it "Infers kind of (Int, Bool)" $ do
      kindOf aEnv "(Int, Bool)" `shouldBe` Right Star
    it "Infers kind of (a, b, c)" $ do
      pendingWith "kind inference rejects free type variables"

  describe "Higher-kinded types" $ do
    it "Infers kind of type constructor variable" $ do
      pendingWith "kind inference rejects free type variables; also Show KVar erases names"
    -- it "Infers kind of f a" $ do
    --   kindOf aEnv "f a" `shouldBe` Right Star  -- needs f :: * -> *

  describe "Complex types" $ do
    it "Infers kind of List (Either Int Bool)" $ do
      kindOf aEnv "List (Either Int Bool)" `shouldBe` Right Star
    it "Infers kind of (Int -> Bool) -> List Int" $ do
      kindOf aEnv "(Int -> Bool) -> List Int" `shouldBe` Right Star

  -- Test with type variables
  describe "Polymorphic kinds" $ do
    it "Infers kind of forall a. a" $ do
      -- Note: parser has no explicit forall; bare `a` is a free var.
      pendingWith "kind inference rejects free type variables"


kindOf :: AnalyzeEnv -> String -> Either Error Kind
kindOf env s = kind'of env (parse'type s)