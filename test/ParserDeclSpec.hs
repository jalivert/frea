module ParserDeclSpec where

import Test.Hspec
import System.Exit

import Compiler.Parser.Parser (parse'expr)
import Compiler.Syntax.Declaration
import Compiler.Syntax.Expression
import Compiler.Syntax.Literal
import Compiler.Syntax.Type


spec :: Spec
spec = describe "Test parsing of the declarations" $ do
  describe "Bindings" $ do
    it "Parses simple constant declaration" $ do
      "module Main where { one = 1 }" <=> Binding "one" (Lit $ LitInt 1)
    
    it "Parses a function declaration" $ do
      "module Main where { fn x = x }" <~> "module Main where { fn = lambda x -> x }"
    
    it "Parses an operator declaration (prefix vs infix equivalence)" $ do
      -- NOTE: `(:)` prefix form is not supported for constructor operators
      -- (LowIdent only allows '(' Op ')', while ':' is opcon). Ordinary
      -- operators like `(+)` do support both forms.
      "module Main where { (+) a b = a }" <~> "module Main where { a + b = a }"

    it "Parses an infix constructor-operator declaration" $ do
      -- NOTE: `:` is opcon, and Binding only supports `Var Op Params`
      -- (TokOperator). Constructor operators cannot be bound; ordinary
      -- operators like `++` support the infix binding form.
      "module Main where { la ++ lb = la }" <=>
        Binding "++" (Lam "la" (Lam "lb" (Var "la")))

    it "Parses annotated function declaration" $ do
      "module Main where { foo :: Int -> Int ; foo x = x }" <=>
        Annotated "foo" (TyArr (TyCon "Int") (TyCon "Int")) (Lam "x" (Var "x"))

    it "Parses multiple declarations" $ do
      "module Main where { a = 1 ; b = 2 }" <=>*
        [ Binding "a" (Lit $ LitInt 1)
        , Binding "b" (Lit $ LitInt 2)
        ]

    it "Parses recursive binding (module-level recursion)" $ do
      -- NOTE: there is no `let rec { ... }` production in the grammar
      -- (TokLetrec is lexed but never used). Recursion is expressed with
      -- plain bindings, which are generalized per SCC.
      -- NOTE 2: frea's infix is n-ary (`AppLeft Oper OneOrMany(AppRight)`),
      -- so the applied arg needs parens: `n * (fact ...)`.
      "module Main where { fact n = if n == 0 then 1 else n * (fact (n - 1)) }" <=>
        Binding "fact" (Lam "n" (If (App (App (Var "==") (Var "n")) (Lit (LitInt 0))) (Lit (LitInt 1)) (App (App (Var "*") (Var "n")) (App (Var "fact") (App (App (Var "-") (Var "n")) (Lit (LitInt 1)))))))

  describe "Data declarations" $ do
    it "Parses a simple data declaration" $ do
      "module Main where { data Bool = True | False }" <=>
        DataDecl "Bool" [] [ConDecl "True" [], ConDecl "False" []]

    it "Parses a polymorphic data declaration" $ do
      "module Main where { data List a = Nil | Cons a (List a) }" <=>
        DataDecl "List" ["a"] [ConDecl "Nil" [], ConDecl "Cons" [TyVar "a", TyApp (TyCon "List") (TyVar "a")]]

    it "Parses data with multiple type parameters" $ do
      "module Main where { data Either a b = Left a | Right b }" <=>
        DataDecl "Either" ["a", "b"] [ConDecl "Left" [TyVar "a"], ConDecl "Right" [TyVar "b"]]

    it "Parses data with constructor arguments" $ do
      "module Main where { data Pair a b = Pair a b }" <=>
        DataDecl "Pair" ["a", "b"] [ConDecl "Pair" [TyVar "a", TyVar "b"]]

  describe "Type synonyms" $ do
    it "Parses a simple type synonym" $ do
      "module Main where { type String = List Char }" <=>
        TypeAlias "String" (TyApp (TyCon "List") (TyCon "Char"))

    it "Parses a polymorphic type synonym" $ do
      -- NOTE: `type Pair a = ...` folds params into TyOp per Parser.y
      "module Main where { type Pair a = (a, a) }" <=>
        TypeAlias "Pair" (TyOp "a" (TyTuple [TyVar "a", TyVar "a"]))

    it "Parses type synonym with function type" $ do
      "module Main where { type Predicate a = a -> Bool }" <=>
        TypeAlias "Predicate" (TyOp "a" (TyArr (TyVar "a") (TyCon "Bool")))

    it "Parses nested type synonyms" $ do
      "module Main where { type A = B ; type B = Int }" <=>*
        [ TypeAlias "A" (TyCon "B")
        , TypeAlias "B" (TyCon "Int")
        ]

  describe "Mixed declarations" $ do
    it "Parses data and functions together" $ do
      "module Main where { data Bool = True | False ; not b = which-Bool b False True }" <=>*
        [ DataDecl "Bool" [] [ConDecl "True" [], ConDecl "False" []]
        , Binding "not" (Lam "b" (App (App (App (Var "which-Bool") (Var "b")) (Var "False")) (Var "True")))
        ]

    it "Parses type synonym and usage" $ do
      "module Main where { type String = List Char ; hello = \"world\" }" <=>*
        [ TypeAlias "String" (TyApp (TyCon "List") (TyCon "Char"))
        , Binding "hello" (App (App (Var ":") (Lit (LitChar 'w'))) (App (App (Var ":") (Lit (LitChar 'o'))) (App (App (Var ":") (Lit (LitChar 'r'))) (App (App (Var ":") (Lit (LitChar 'l'))) (App (App (Var ":") (Lit (LitChar 'd'))) (Var "[]"))))))
        ]

infix 4 <=>

(<=>) :: String -> Declaration -> IO ()
(<=>) expr reference = do
  case parse'expr expr of
    Left [decl] -> show decl `shouldBe` show reference
    Right ast' -> exitFailure

infix 4 <=>*

(<=>*) :: String -> [Declaration] -> IO ()
(<=>*) expr reference = do
  case parse'expr expr of
    Left decls -> map show decls `shouldBe` map show reference
    Right ast' -> exitFailure


infix 4 <~>

(<~>) :: String -> String -> IO ()
(<~>) expr'l expr'r = do
  case (parse'expr expr'l, parse'expr expr'r) of
    (l, r) -> show l `shouldBe` show r


oK :: String -> IO ()
oK expr = do
  case parse'expr expr of
    Left decls -> return ()