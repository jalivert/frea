module ModuleSpec where

import Test.Hspec
import System.Exit

import qualified Data.Map.Strict as Map

import Compiler.Parser.Parser (parse'expr, parse'type)
import Compiler.Syntax.Declaration
import Compiler.Syntax.Expression
import Compiler.Syntax.Type
import Compiler.Syntax.Kind (Kind(..))
import Compiler.Syntax.Literal

import Compiler.TypeAnalyzer.TypeOf (analyze'module)
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.Error
import Compiler.TypeAnalyzer.Types
import Compiler.TypeAnalyzer.Kind.KindOf (kind'of)

import Interpreter.Value (empty'env, empty'memory)
import Interpreter.Evaluate
import Interpreter.Address (Address)
import qualified Interpreter.Value as Val

-- Helper to check Either without requiring Show instance
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

checkRight :: Show a => Either a b -> Expectation
checkRight (Right _) = pure ()
checkRight (Left err) = expectationFailure $ "Expected Right, got Left: " ++ show err

-- Mock environment for testing
testEnv :: Map.Map String Address
testEnv = Map.empty

testMemory :: Val.Memory
testMemory = Map.empty


spec :: Spec
spec = describe "Test module loading and analysis" $ do
  describe "Module parsing" $ do
    it "Parses module with data and functions" $ do
      "module Main where { data Bool = True | False ; not b = which-Bool b False True }" <=>*
        [ DataDecl "Bool" [] [ConDecl "True" [], ConDecl "False" []]
        , Binding "not" (Lam "b" (App (App (App (Var "which-Bool") (Var "b")) (Var "False")) (Var "True")))
        ]

    it "Parses module with type synonyms" $ do
      "module Main where { type String = List Char ; hello = \"world\" }" <=>*
        [ TypeAlias "String" (TyApp (TyCon "List") (TyCon "Char"))
        , Binding "hello" (App (App (Var ":") (Lit (LitChar 'w'))) (App (App (Var ":") (Lit (LitChar 'o'))) (App (App (Var ":") (Lit (LitChar 'r'))) (App (App (Var ":") (Lit (LitChar 'l'))) (App (App (Var ":") (Lit (LitChar 'd'))) (Var "[]"))))))
        ]

  describe "Module type analysis" $ do
    let kEnv = empty'k'env `Map.union` Map.fromList
          [ ("Bool", Star)
          , ("List", KArr Star Star)
          , ("Char", Star)
          , ("Int", Star)
          , ("Maybe", KArr Star Star)
          , ("Either", KArr Star (KArr Star Star))
          ]
        tEnv = empty't'env `Map.union` Map.fromList
          [ ("True", ForAll [] (TyCon "Bool"))
          , ("False", ForAll [] (TyCon "Bool"))
          , ("[]", ForAll ["a"] (TyApp (TyCon "List") (TyVar "a")))
          -- NOTE: backticked TyArr chains associate LEFT (`(a->b)->c`);
          -- function types need explicit right nesting.
          , (":", ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyApp (TyCon "List") (TyVar "a")))))
          , ("which-Bool", ForAll ["r"] (TyArr (TyCon "Bool") (TyArr (TyVar "r") (TyArr (TyVar "r") (TyVar "r")))))
          , ("which-List", ForAll ["a", "r"] (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyArr (TyVar "r") (TyArr (TyArr (TyVar "a") (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyVar "r"))) (TyVar "r")))))
          , ("which-Maybe", ForAll ["a", "r"] (TyArr (TyApp (TyCon "Maybe") (TyVar "a")) (TyArr (TyVar "r") (TyArr (TyArr (TyVar "a") (TyVar "r")) (TyVar "r")))))
          , ("#+", ForAll [] (TyArr (TyTuple [TyCon "Int", TyCon "Int"]) (TyCon "Int")))
          , ("#=", ForAll ["a"] (TyArr (TyTuple [TyVar "a", TyVar "a"]) (TyCon "Bool")))
          ]
        aliEnv = Map.fromList
          [ ("String", TyApp (TyCon "List") (TyCon "Char"))
          ]
        aEnv = AEnv kEnv tEnv aliEnv

    it "Analyzes simple data declaration" $ do
      let decls = [DataDecl "Bool" [] [ConDecl "True" [], ConDecl "False" []]]
      checkRight $ run'analyze aEnv (analyze'module decls (empty'env, testMemory))

    it "Analyzes data with functions" $ do
      let decls = 
            [ DataDecl "Bool" [] [ConDecl "True" [], ConDecl "False" []]
            , Binding "not" (Lam "b" (App (App (App (Var "which-Bool") (Var "b")) (Var "False")) (Var "True")))
            ]
      checkRight $ run'analyze aEnv (analyze'module decls (empty'env, testMemory))

    it "Analyzes type synonyms" $ do
      let decls = [TypeAlias "String" (TyApp (TyCon "List") (TyCon "Char"))]
      checkRight $ run'analyze aEnv (analyze'module decls (empty'env, testMemory))

    it "Analyzes recursive functions" $ do
      -- NOTE: `#=`/`#*`/`#-` take a single tuple argument; the previous
      -- hand-built AST applied them curried (`App (App (Var \"#=\") n) ...`),
      -- which cannot typecheck. Use the tuple form like real programs do.
      let decls =
            [ Binding "fact" (Lam "n" (If (App (Op "#=") (Tuple [Var "n", Lit (LitInt 0)])) (Lit (LitInt 1)) (App (Op "#*") (Tuple [Var "n", App (Var "fact") (App (Op "#-") (Tuple [Var "n", Lit (LitInt 1)]))])))) ]
      checkRight $ run'analyze aEnv (analyze'module decls (empty'env, testMemory))

    it "Analyzes map function" $ do
      let whichList = Var "which-List"
          nilCase = Var "[]"
          consCase = Lam "h" (Lam "t" (App (App (Var ":") (App (Var "f") (Var "h"))) (App (App (Var "map") (Var "f")) (Var "t"))))
          mapBody = Lam "lst" (App (App (App whichList (Var "lst")) nilCase) consCase)
          decls = [ Binding "map" (Lam "f" mapBody) ]
      checkRight $ run'analyze aEnv (analyze'module decls (empty'env, testMemory))

    it "Analyzes polymorphic functions" $ do
      let decls = 
            [ Binding "id" (Lam "x" (Var "x"))
            , Binding "const" (Lam "x" (Lam "y" (Var "x")))
            ]
      checkRight $ run'analyze aEnv (analyze'module decls (empty'env, testMemory))

  describe "Kind inference for modules" $ do
    -- NOTE: empty'k'env only has Bool/Int/Double/Char/Unit; List/Either/Maybe
    -- must come from the module kEnv. Use it here.
    let kEnv' = empty'k'env `Map.union` Map.fromList
          [ ("List", KArr Star Star)
          , ("Maybe", KArr Star Star)
          , ("Either", KArr Star (KArr Star Star))
          ]
        kEnvOf = AEnv kEnv' empty't'env empty'ali'env
    it "Infers kind of data type" $ do
      let ty = parse'type "Bool"
      kind'of kEnvOf ty `shouldBe` Right Star

    it "Infers kind of List" $ do
      let ty = parse'type "List"
      kind'of kEnvOf ty `shouldBe` Right (KArr Star Star)

    it "Infers kind of List Int" $ do
      let ty = parse'type "List Int"
      kind'of kEnvOf ty `shouldBe` Right Star

    it "Infers kind of Either" $ do
      let ty = parse'type "Either"
      kind'of kEnvOf ty `shouldBe` Right (KArr Star (KArr Star Star))

    it "Infers kind of function type" $ do
      let ty = parse'type "Int -> Bool"
      kind'of kEnvOf ty `shouldBe` Right Star

    it "Infers kind of tuple" $ do
      let ty = parse'type "(Int, Bool, Char)"
      kind'of kEnvOf ty `shouldBe` Right Star

  describe "Prelude loading" $ do
    it "Loads prelude.frea without errors" $ do
      -- This would require loading the actual prelude.frea file
      -- For now, we test that the parsing works
      pending

infix 4 <=>*

(<=>*) :: String -> [Declaration] -> IO ()
(<=>*) expr reference = do
  case parse'expr expr of
    Left decls -> map show decls `shouldBe` map show reference
    Right ast' -> exitFailure