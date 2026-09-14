module Compiler.TypeAnalyzer.Error where

import Data.List

import Compiler.Syntax (Type, Scheme, Kind)


data Error
  = InfiniteType Type Type
  | InfiniteKind Kind Kind
  | TypeUnifMismatch Type Type
  | KindUnifMismatch Kind Kind
  | UnboundVar String
  | UnboundTypeVar String
  | TypeShapeMismatch Type Type
  | KindShapeMismatch Kind Kind
  | TypeUnifCountMismatch [Type] [Type]
  | KindUnifCountMismatch [Kind] [Kind]
  | SynonymCycle [(String, Type)]

  | AnnotationTooGeneral String Type
  -- ^ An annotation variable (first argument, user-written name) had to
  -- unify with the second argument, so the annotation is more general
  -- than the definition it annotates.

  | Unexpected String
  deriving (Eq)


instance Show Error where
  show (InfiniteType type'a type'b)
    = "Occurs check: cannot construct the infinite type:\n  "
      ++ show type'a ++ " ~ " ++ show type'b
  show (InfiniteKind kind'a kind'b)
    = "Occurs check: cannot construct the infinite kind:\n  "
      ++ show kind'a ++ " ~ " ++ show kind'b
  show (TypeUnifMismatch type'a type'b)
    = "Couldn't match type `" ++ show type'a ++ "` with `" ++ show type'b ++ "`"
  show (KindUnifMismatch kind'a kind'b)
    = "Couldn't match kind `" ++ show kind'a ++ "` with `" ++ show kind'b ++ "`"
  show (UnboundVar name)
    = "Unknown variable " ++ name
  show (UnboundTypeVar name)
    = "Unknown type variable " ++ name
  show (TypeShapeMismatch type'l type'r)
    = "[Shape] Couldn't match type `" ++ show type'l ++ "` with `" ++ show type'r ++ "`"
  show (KindShapeMismatch kind'l kind'r)
    = "[Shape] Couldn't match kind `" ++ show kind'l ++ "` with `" ++ show kind'r ++ "`"
  show (SynonymCycle aliases)
    = "Found a cycle in the type synonym declaration(s) of\n" ++ intercalate "\n" (map prnt aliases)
      where
        prnt (name, type') = "  type " ++ name ++ " = " ++ show type'
  show (AnnotationTooGeneral var type')
    = "The type annotation is too general: variable `" ++ var
      ++ "` cannot match `" ++ show type' ++ "`"
  show (Unexpected s)
    = "Something bad happened: " ++ s
