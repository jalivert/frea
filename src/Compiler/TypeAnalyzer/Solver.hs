{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.TypeAnalyzer.Solver where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Functor.Identity

import Control.Monad.Except

import Compiler.Syntax.Type
import Compiler.Syntax.Kind

import Compiler.TypeAnalyzer.Error
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.AnalyzeState
import Compiler.TypeAnalyzer.Substituable
import Compiler.TypeAnalyzer.Constraint
import Compiler.TypeAnalyzer.AnalyzeEnv


-- Constraint solver monad
type Solve a = ExceptT Error Identity a


type Unifier a = (Subst a, [Constraint a])


class Unifiable a where
  unify :: RigidVars -> a -> a -> Solve (Subst a)


class UnifiableComb a b where
  unify'many :: RigidVars -> a b -> a b -> Solve (Subst b)


class Occurable a where
  occurs'in :: String -> a -> Bool


class Bindable a where
  bind :: RigidVars -> String -> a -> Solve (Subst a)


run'solve :: ((Substitutable a a), (Unifiable a)) => RigidVars -> [Constraint a] -> Either Error (Subst a)
run'solve rig constrs = runIdentity $ runExceptT $ solver rig state
  where state = (empty'subst, constrs)


-- Unification solver
solver :: ((Substitutable a a), (Unifiable a)) => RigidVars -> Unifier a -> Solve (Subst a)
solver rig (subst, constraints) =
  case constraints of
    [] -> return subst
    ((type'l, type'r) : constrs) -> do
      subst'  <- unify rig type'l type'r
      solver rig (subst' `compose` subst, apply subst' constrs)


instance Unifiable Type where
  unify _ t1 t2 | t1 == t2 = return empty'subst
  unify rig (TyVar v) t = bind rig v t
  unify rig t (TyVar v) = bind rig v t
  unify rig (TyArr t1 t2) (TyArr t3 t4) = unify'many rig [t1, t2] [t3, t4]
  unify rig (TyApp t1 t2) (TyApp t3 t4) = unify'many rig [t1, t2] [t3, t4]
  unify _ l@(TyCon name'l) r@(TyCon name'r)
    | name'l == name'r = return empty'subst
    | otherwise = throwError $ TypeUnifMismatch l r
  unify rig (TyTuple ts'left) (TyTuple ts'right)
    = if length ts'left /= length ts'right
      then throwError $ TypeShapeMismatch (TyTuple ts'left) (TyTuple ts'right)
      else unify'many rig ts'left ts'right
  unify _ t1 t2 = throwError $ TypeShapeMismatch t1 t2


instance Unifiable Kind where
  unify _ t1 t2 | t1 == t2 = return empty'subst
  unify _ (KVar v) k = bind Map.empty v k
  unify _ k (KVar v) = bind Map.empty v k
  unify _ (KArr k1 k2) (KArr k3 k4) = unify'many Map.empty [k1, k2] [k3, k4]
  unify _ Star Star = return empty'subst
  unify _ t1 t2 = throwError $ KindShapeMismatch t1 t2


instance UnifiableComb [] Type where
  unify'many _ [] [] = return empty'subst
  unify'many rig (t'l : ts'l) (t'r : ts'r) = do
    su1 <- unify rig t'l t'r
    su2 <- unify'many rig (apply su1 ts'l) (apply su1 ts'r)
    return (su2 `compose` su1)
  unify'many _ t'l t'r = throwError $ TypeUnifCountMismatch t'l t'r


instance UnifiableComb [] Kind where
  unify'many _ [] [] = return empty'subst
  unify'many rig (t'l : ts'l) (t'r : ts'r) = do
    su1 <- unify rig t'l t'r
    su2 <- unify'many rig (apply su1 ts'l) (apply su1 ts'r)
    return (su2 `compose` su1)
  unify'many _ t'l t'r = throwError $ KindUnifCountMismatch t'l t'r


compose :: Substitutable a a => Subst a -> Subst a -> Subst a
(Sub sub'l) `compose` (Sub sub'r)
  = Sub $ Map.map (apply (Sub sub'l)) sub'r `Map.union` sub'l


instance Occurable Type where
  name `occurs'in` (TyVar varname)
    = name == varname
  name `occurs'in` (TyCon conname)
    = name == conname -- TODO: So I think I can do that safely. Consider if TyCon didn't exist and everything would just be a TyVar. You would do this check and by the fact that constructors start with upper case letter it wouldn't break anything. 
  name `occurs'in` (TyTuple ts)
    = any (name `occurs'in`) ts
  name `occurs'in` (TyArr left right)
    = name `occurs'in` left || name `occurs'in` right
  name `occurs'in` (TyApp left right)
    = name `occurs'in` left || name `occurs'in` right


instance Occurable Kind where
  name `occurs'in` (KVar varname)
    = name == varname
  name `occurs'in` Star
    = False
  name `occurs'in` (KArr left right)
    = name `occurs'in` left || name `occurs'in` right


instance Bindable Type where
  bind rig varname type'
    | type' == TyVar varname = return empty'subst
    | Just display <- Map.lookup varname rig =
        -- A rigid (annotation) variable unifies only with itself. Binding a
        -- flexible variable to it is fine; anything else means the annotation
        -- promises more polymorphism than the definition delivers.
        case type' of
          TyVar other | Map.notMember other rig ->
            return $ Sub $ Map.singleton other (TyVar varname)
          _ -> throwError $ AnnotationTooGeneral display (display'rigid rig type')
    | varname `occurs'in` type' = throwError $ InfiniteType (TyVar varname) type'
    | otherwise                 = return $ Sub $ Map.singleton varname type'


instance Bindable Kind where
  bind _ varname kind'
    | kind' == KVar varname     = return empty'subst
    | varname `occurs'in` kind' = throwError $ InfiniteKind (KVar varname) kind'
    | otherwise                 = return $ Sub $ Map.singleton varname kind'


-- | Render a type for annotation errors, showing rigid variables by the
-- user-written name they were skolemized from.
display'rigid :: RigidVars -> Type -> Type
display'rigid rig type' = case type' of
  TyVar name -> TyVar $ Map.findWithDefault name name rig
  TyCon name -> TyCon name
  TyTuple types -> TyTuple $ map (display'rigid rig) types
  TyArr left right -> TyArr (display'rigid rig left) (display'rigid rig right)
  TyApp left right -> TyApp (display'rigid rig left) (display'rigid rig right)
  TyOp par body -> TyOp par (display'rigid rig body)
