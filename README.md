# Frea

[![CI](https://github.com/jalivert/frea/workflows/CI/badge.svg)](https://github.com/jalivert/frea/actions/workflows/ci.yml)

Frea is a small, lazily evaluated functional language with
Damas–Hindley–Milner type inference, algebraic data types, and a kind
system — implemented in Haskell, from lexer to interpreter.

```haskell
frea λ > let { fact = \ n -> if n == 0 then 1 else n * (fact (n - 1)) } in fact 5

         120
```

## Highlights

- **HM type inference** with let-polymorphism (constraint generation + unification)
- **Algebraic data types** eliminated via generated `which-<Type>` eliminators
- **Kind inference**, including higher-kinded types (`List :: * -> *`)
- **Type synonyms** with cycle detection
- **Lazy evaluation** with thunk memoization (infinite lists work)
- **REPL** with `:t` (infer type) and `:k` (infer kind) commands
- **262-example test suite** covering parsing, inference, kinds, synonyms, modules, and evaluation

## Quick start

Requires GHC and Cabal.

```sh
cabal build   # build the library and REPL
cabal run frea-exe   # start the REPL (loads prelude.frea)
cabal test    # run the test suite
```

## Language tour

All examples below are real REPL sessions.

### Literals

```haskell
frea λ > :t -23

         -23 :: Int

frea λ > :t 23.23

         23.23 :: Double

frea λ > :t 'c'

         'c' :: Char

frea λ > :t "hello"

         "hello" :: List Char
```

Strings are lists of characters: `"hello" :: List Char`.

### Lists, tuples, unit

```haskell
frea λ > :t [1, 2, 3]

         [1, 2, 3] :: List Int

frea λ > :t (1, "s", 'c')

         (1, "s", 'c') :: (Int, List Char, Char)

frea λ > :t ()

         () :: ()
```

### Functions and operators

Lambdas use `\` (with a trailing space) or the `lambda` keyword, and take
one argument each — multi-argument functions are curried.

```haskell
frea λ > :t (\ x -> x + 1)

         (\ x -> x + 1) :: Int -> Int

frea λ > :t (+)

         (+) :: Int -> Int -> Int
```

Operators work infix, prefix, and in backticks; new ones are defined like
ordinary functions:

```haskell
frea λ > let { a `plus` b = a + b } in 23 `plus` 42

         65
```

### Let bindings and polymorphism

```haskell
frea λ > let { f = \ x -> x } in (f 1, f True)

         (1, True)
```

`f` is inferred polymorphic and instantiated at `Int` and `Bool` in the
same expression — let-polymorphism in action.

### Conditionals and recursion

```haskell
frea λ > if True then 23 else 42

         23

frea λ > let { fact = \ n -> if n == 0 then 1 else n * (fact (n - 1)) } in fact 5

         120
```

### Custom data types

Declare types with `data`; constructors and a `which-<Type>` eliminator are
generated for each one. The prelude defines `Bool`, `Maybe`, `Either`, and
`List` this way:

```haskell
data List a
  = []
  | a : (List a)
```

The eliminator takes the value plus one branch per constructor, in
declaration order:

```haskell
frea λ > which-Maybe (Just 42) 0 (\ x -> x + 1)

         43

frea λ > let { double = \ x -> 2 * x
             ; map = \ f lst -> which-List lst [] (\ h t -> (f h) : (map f t)) }
         in map double [1, 2, 3]

         [2, 4, 6]
```

### Type annotations

Top-level bindings and parenthesized expressions can be annotated:

```haskell
module Main where
{ len :: List a -> Int
; len lst = which-List lst 0 (\ h t -> 1 + (len t)) }
```

```haskell
frea λ > :t ((\ x -> x) :: Int -> Int)

         ((\ x -> x) :: Int -> Int) :: Int -> Int
```

### Laziness

Evaluation is lazy and memoized, so infinite structures are fine:

```haskell
frea λ > let { ones = 1 : ones
             ; take = \ n lst -> if n == 0 then []
                                 else which-List lst [] (\ h t -> h : (take (n - 1) t)) }
         in take 5 ones

         [1, 1, 1, 1, 1]
```

## REPL commands

| Input | Effect |
|---|---|
| *expression* | Type-check, evaluate, and print |
| `:t` *expression* | Show the inferred type without evaluating |
| `:k` *type* | Show the inferred kind, e.g. `:k Int -> Bool` gives `*` |
| `:load` *file* | Load a module file |
| `:exit`, `:q` | Quit |

Expressions may span multiple lines; submit an empty line to evaluate.
Submit module-style bindings (`module Name where { ... }`) to extend the
global environment.

## Repository layout

```
src/
  Compiler/Parser/        Lexer (Alex) + grammar (Happy)
  Compiler/Syntax/        AST: expressions, types, kinds, declarations
  Compiler/TypeAnalyzer/  Kind inference, HM inference, solver, synonyms
  Interpreter/            Lazy evaluator (thunks + memoized memory)
app/Main.hs               REPL
prelude.frea              Standard library (Bool, List, Maybe, arithmetic…)
test/                     Hspec suite: parser, inference, kinds, eval, …
examples/                 Sample programs
```

## Scope and limitations

Frea is a learning project, deliberately small. There is intentionally no
module system beyond single-file loading, no pattern-match syntax (use the
`which-*` eliminators), no type classes, and no exhaustiveness checking.
Parse errors are reported plainly without recovery. Within that scope, the
typechecker and evaluator are well tested — see `test/`.
