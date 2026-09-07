module TestUtils where

import Test.Hspec
import System.Exit

import qualified Data.Map.Strict as Map

import Compiler.Parser.Parser (parse'expr)
import Compiler.Syntax.Expression
import Compiler.Syntax.Type
import Compiler.Syntax.Declaration
import Compiler.Syntax.Literal

import Compiler.TypeAnalyzer.TypeOf (infer'expression)
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.Error
import Compiler.TypeAnalyzer.Types
import Compiler.TypeAnalyzer.Kind.KindOf (kind'of)

import Interpreter.Value (empty'env, empty'memory)
import Interpreter.Evaluate (force)
import Interpreter.Value (Value(..))

import Control.Monad.State.Lazy


-- | Standard kind environment for testing
stdKEnv :: Map.Map String Kind
stdKEnv = Map.fromList
  [ ("()", Star)
  , ("Bool", Star)
  , ("Int", Star)
  , ("Double", Star)
  , ("Char", Star)
  , ("List", KArr Star Star)
  , ("Maybe", KArr Star Star)
  , ("Either", KArr Star (KArr Star Star))
  , ("->", KArr Star (KArr Star Star))
  , (",", KArr Star (KArr Star Star))  -- tuple
  ]

-- | Standard type environment for testing
stdTEnv :: TypeEnv
stdTEnv = Map.fromList
  [ ("()", ForAll [] (TyCon "()"))
  , ("True", ForAll [] (TyCon "Bool"))
  , ("False", ForAll [] (TyCon "Bool"))
  , ("[]", ForAll ["a"] (TyApp (TyCon "List") (TyVar "a")))
  , (":", ForAll ["a"] (TyArr (TyVar "a") (TyArr (TyApp (TyCon "List") (TyVar "a")) (TyApp (TyCon "List") (TyVar "a")))))
  , ("#+", ForAll [] (TyArr (TyTuple [TyCon "Int", TyCon "Int"]) (TyCon "Int")))
  , ("#+.", ForAll [] (TyTuple [TyCon "Double", TyCon "Double"] `TyArr` TyCon "Double"))
  , ("#-", ForAll [] (TyTuple [TyCon "Int", TyCon "Int"] `TyArr` TyCon "Int"))
  , ("#-.", ForAll [] (TyTuple [TyCon "Double", TyCon "Double"] `TyArr` TyCon "Double"))
  , ("#*", ForAll [] (TyTuple [TyCon "Int", TyCon "Int"] `TyArr` TyCon "Int"))
  , ("#*.", ForAll [] (TyTuple [TyCon "Double", TyCon "Double"] `TyArr` TyCon "Double"))
  , ("#div", ForAll [] (TyTuple [TyCon "Int", TyCon "Int"] `TyArr` TyCon "Int"))
  , ("#/", ForAll [] (TyTuple [TyCon "Double", TyCon "Double"] `TyArr` TyCon "Double"))
  , ("#=", ForAll ["a"] (TyTuple [TyVar "a", TyVar "a"] `TyArr` TyCon "Bool"))
  , ("#<", ForAll ["a"] (TyTuple [TyVar "a", TyVar "a"] `TyArr` TyCon "Bool"))
  , ("#>", ForAll ["a"] (TyTuple [TyVar "a", TyVar "a"] `TyArr` TyCon "Bool"))
  , ("#fst", ForAll ["a", "b"] (TyTuple [TyVar "a", TyVar "b"] `TyArr` TyVar "a"))
  , ("#snd", ForAll ["a", "b"] (TyTuple [TyVar "a", TyVar "b"] `TyArr` TyVar "b"))
  , ("#show", ForAll ["a"] (TyVar "a" `TyArr` TyApp (TyCon "List") (TyCon "Char")))
  , ("#debug", ForAll ["a"] (TyVar "a" `TyArr` TyVar "a"))
  , ("which-Bool", ForAll ["r"] (TyCon "Bool" `TyArr` TyVar "r" `TyArr` TyVar "r" `TyArr` TyVar "r"))
  , ("which-List", ForAll ["a", "r"] (TyApp (TyCon "List") (TyVar "a") `TyArr` TyVar "r" `TyArr` (TyVar "a" `TyArr` TyApp (TyCon "List") (TyVar "a") `TyArr` TyVar "r") `TyArr` TyVar "r"))
  , ("which-Maybe", ForAll ["a", "r"] (TyApp (TyCon "Maybe") (TyVar "a") `TyArr` TyVar "r" `TyArr` (TyVar "a" `TyArr` TyVar "r") `TyArr` TyVar "r"))
  , ("which-Either", ForAll ["a", "b", "r"] (TyApp (TyApp (TyCon "Either") (TyVar "a")) (TyVar "b") `TyArr` (TyVar "a" `TyArr` TyVar "r") `TyArr` (TyVar "b" `TyArr` TyVar "r") `TyArr` TyVar "r"))
  , ("fix", ForAll ["a"] ((TyVar "a" `TyArr` TyVar "a") `TyArr` TyVar "a"))
  ]

-- | Standard alias environment for testing
stdAliEnv :: Map.Map String Type
stdAliEnv = Map.fromList
  [ ("String", TyApp (TyCon "List") (TyCon "Char"))
  , ("Nat", TyCon "Int")
  ]

-- | Standard analysis environment for testing
stdAEnv :: AnalyzeEnv
stdAEnv = AEnv stdKEnv stdTEnv stdAliEnv

-- | Parse and infer type of expression
inferType :: String -> Either Error Scheme
inferType expr = do
  case parse'expr expr of
    Left _ -> Left (ErrorMsg "Parse error: expected expression, got declarations")
    Right ast -> run'analyze stdAEnv (infer'expression ast)

-- | Parse and infer kind of type
inferKind :: String -> Either Error Kind
inferKind tyStr = do
  let ty = parse'type tyStr
  kind'of stdAEnv ty

-- | Evaluate expression
evalExpr :: String -> Either String Value
evalExpr expr = do
  case parse'expr expr of
    Left _ -> Left "Parse error: expected expression, got declarations"
    Right ast -> do
      let (result, _) = runState (force ast empty'env) empty'memory
      case result of
        Left err -> Left $ show err
        Right val -> Right val

-- | Parse declarations
parseDecls :: String -> Either String [Declaration]
parseDecls input = case parse'expr input of
  Left decls -> Right decls
  Right _ -> Left "Expected declarations, got expression"

-- | Run analysis on declarations
analyzeDecls :: [Declaration] -> Either Error (AnalyzeEnv, Interpreter.Value.Env, Interpreter.Value.Memory)
analyzeDecls decls = run'analyze stdAEnv (analyze'module decls (empty'env, empty'memory))

-- | Check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Check if Either is Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- | Type synonyms for common types
tInt :: Type
tInt = TyCon "Int"

tDouble :: Type
tDouble = TyCon "Double"

tChar :: Type
tChar = TyCon "Char"

tBool :: Type
tBool = TyCon "Bool"

tUnit :: Type
tUnit = TyCon "()"

tList :: Type -> Type
tList a = TyApp (TyCon "List") a

tMaybe :: Type -> Type
tMaybe a = TyApp (TyCon "Maybe") a

tEither :: Type -> Type -> Type
tEither a b = TyApp (TyApp (TyCon "Either") a) b

tArr :: Type -> Type -> Type
tArr = TyArr

tTuple :: [Type] -> Type
tTuple = TyTuple

tVar :: String -> Type
tVar = TyVar

tCon :: String -> Type
tCon = TyCon

-- | Value constructors for testing
vInt :: Int -> Value
vInt = VInt

vDouble :: Double -> Value
vDouble = VDouble

vChar :: Char -> Value
vChar = VChar

vBool :: Bool -> Value
vBool True = VData "True" []
vBool False = VData "False" []

vUnit :: Value
vUnit = VUnit

vList :: [Value] -> Value
vList = VList

vData :: String -> [Value] -> Value
vData = VData

vTuple :: [Value] -> Value
vTuple = VTuple