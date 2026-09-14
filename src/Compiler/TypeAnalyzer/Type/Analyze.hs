{-# LANGUAGE TupleSections #-}
module Compiler.TypeAnalyzer.Type.Analyze where


import Data.Graph (SCC(..), stronglyConnComp)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Functor.Identity
import Data.Bifunctor (second)

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Compiler.Syntax.Expression
import Compiler.Syntax.Type
import Compiler.Syntax.Kind
import Compiler.Syntax.Literal

import Compiler.TypeAnalyzer.Types
import Compiler.TypeAnalyzer.Error
import Compiler.TypeAnalyzer.Analyze
import Compiler.TypeAnalyzer.Solver
import Compiler.TypeAnalyzer.Substituable
import Compiler.TypeAnalyzer.AnalyzeEnv
import Compiler.TypeAnalyzer.Constraint
import Compiler.TypeAnalyzer.AnalyzeState
import Compiler.TypeAnalyzer.AnalyzeUtils
import Compiler.TypeAnalyzer.Dependency


import qualified Compiler.TypeAnalyzer.Kind.Infer as K


check :: Type -> Expression -> Analyze ((), [Constraint Type], [Constraint Kind])
check t (Lit (LitInt _)) = return ((), [(t, t'Int)], [])
check t (Lit (LitDouble _)) = return ((), [(t, t'Double)], [])
check t (Lit (LitChar _)) = return ((), [(t, t'Char)], [])

check t (Ann type' expr) = do
  (t'ann, constrs, k'cs) <- infer (Ann type' expr)
  return ((), (t, t'ann) : constrs, k'cs)

check t (Var x) = do
  -- for now just assume t is valid type of kind *
  type' <- lookup't'env x
  return ((), [(t, type')], [])

check t (Op x) = do
  -- assume t :: *
  type' <- lookup't'env x
  return ((), [(t, type')], [])

check t (Lam x body) = do
  -- assume t :: *; relating it to an arrow exposes a mismatch (or an
  -- over-general annotation) at solve time instead of crashing here
  from <- TyVar <$> fresh
  to <- TyVar <$> fresh
  ((), constrs, k'constrs) <- put'in't'env (x, ForAll [] from) (check to body)
  return ((), (t, from `TyArr` to) : constrs, k'constrs)

check t (App left right) = do
  -- assume t :: *
  (t'l, cs'l, k'cs'l) <- infer left
  (t'r, cs'r, k'cs'r) <- infer right
  fresh'name <- fresh 
  let t'var = TyVar fresh'name
  return ((), (t, t'var) : (t'l, t'r `TyArr` t'var) : cs'l ++ cs'r, k'cs'l ++ k'cs'r)

check t (If cond tr fl) = do
  -- assume t :: *
  (t1, c1, k'c1) <- infer cond
  ((), c2, k'c2) <- check t tr
  ((), c3, k'c3) <- check t fl
  return ((), (t1, t'Bool) : c1 ++ c2 ++ c3, k'c1 ++ k'c2 ++ k'c3)

check t (Let bind'pairs ex'body) = do
  (t'env', t'constrs, k'constrs) <- infer'definitions bind'pairs
  ((), cs'body, k'cs'body) <- local (\ e@AEnv{ } -> e{ type'env = t'env' }) (check t ex'body)
      
  return ((), t'constrs ++ cs'body, k'constrs ++ k'cs'body)

check t (Tuple exprs) = do
  -- assume t :: *; relating it to a tuple exposes length and shape
  -- mismatches at solve time instead of truncating or crashing here
  freshs <- mapM (\ _ -> TyVar <$> fresh) exprs
  (cs, k'cs) <- foldM check' ([], []) (zip freshs exprs)
  return ((), (t, TyTuple freshs) : cs, k'cs)
    where
      check' (constrs, k'constrs) (ty, expr) = do
        ((), cs, k'cs) <- check ty expr
        return (cs ++ constrs, k'constrs ++ k'cs)


infer :: Expression -> Analyze (Type, [Constraint Type], [Constraint Kind])
infer expr = case expr of  
  Lit (LitInt i) -> return (t'Int, [], [])
  Lit (LitDouble d) -> return (t'Double, [], [])
  Lit (LitChar ch) -> return (t'Char, [], [])

  (Var x) -> do
    type' <- lookup't'env x
    return (type', [], [])

  Op x -> do
    type' <- lookup't'env x
    return (type', [], [])

  Lam x body -> do
    fresh'name <- fresh
    let t'var = TyVar fresh'name
    (t, t'constrs, k'constrs) <- put'in't'env (x, ForAll [] t'var) (infer body)
    return (t'var `TyArr` t, t'constrs, k'constrs)

  App left right -> do
    (t'l, cs'l, k'cs'l) <- infer left
    (t'r, cs'r, k'cs'r) <- infer right
    fresh'name <- fresh
    let t'var = TyVar fresh'name
    return (t'var, cs'l ++ cs'r ++ [(t'l, t'r `TyArr` t'var)], k'cs'l ++ k'cs'r)

  If cond tr fl -> do
    (t1, c1, k'c1) <- infer cond
    (t2, c2, k'c2) <- infer tr
    (t3, c3, k'c3) <- infer fl
    return (t2, (t1, t'Bool) : (t2, t3) : c1 ++ c2 ++ c3, k'c1 ++ k'c2 ++ k'c3)
  
  Let bind'pairs ex'body -> do
    (t'env', t'constrs, k'constrs) <- infer'definitions bind'pairs
    (t'body, cs'body, k'cs'body) <- local (\ e@AEnv{ } -> e{ type'env = t'env' }) (infer ex'body)
        
    return (t'body, t'constrs ++ cs'body, k'constrs ++ k'cs'body)

  Tuple exprs -> do
    (types, cs, k'cs) <- foldM infer' ([], [], []) exprs
    return (TyTuple $ reverse types, cs, k'cs)
      where
        infer' (types, constrs, k'constrs) expr = do
          (t, cs, k'cs) <- infer expr
          return (t : types, cs ++ constrs, k'cs ++ k'constrs)
  
  Ann type' expr -> do
    -- Annotation variables become rigid skolems: checking against them
    -- rejects over-general annotations instead of silently specializing.
    let scheme = generalize empty't'env type'
    t'sk <- skolemize scheme
    pairs <- mapM (\ name -> (name,) <$> (KVar <$> fresh)) (Set.toList $ free'vars t'sk)
    (kind, k'constrs) <- merge'into'k'env pairs (K.infer t'sk)
    (_, constrs, k'cs) <- check t'sk expr
    return (t'sk, constrs, (kind, Star) : k'constrs ++ k'cs)


-- NOTE: maybe it should stay here
infer'definitions :: [(String, Expression)] -> Analyze (TypeEnv, [Constraint Type], [Constraint Kind])
infer'definitions bindings = do
  let indexed = index'bindings bindings
  let graph = build'graph bindings indexed
  let solved = stronglyConnComp graph

  infer'groups solved
    where
      infer'groups :: [SCC (String, Expression)] -> Analyze (TypeEnv, [Constraint Type], [Constraint Kind])
      infer'groups [] = do
        t'e' <- asks type'env
        return (t'e', [], [])

      infer'groups ((AcyclicSCC bind) : sccs) = do
        ((bind'name, bind'type), t'constrs, k'constrs) <- infer'one bind

        -- ted to musim solvnout a zapracovat a infernout zbytek sccs
        rig <- get'rigid
        case run'solve rig t'constrs of
          Left err -> throwError err
          Right subst -> do
            (t'env', t'constrs', k'constrs') <- put'in't'env (bind'name, closeOver $ apply subst bind'type) (infer'groups sccs)
            return (t'env', t'constrs ++ t'constrs', k'constrs ++ k'constrs')

      infer'groups ((CyclicSCC bindings) : sccs) = do
        (t'binds, t'constrs, k'constrs) <- infer'group bindings

        rig <- get'rigid
        case run'solve rig t'constrs of
          Left err -> throwError err
          Right subst -> do
            (t'env', t'constrs', k'constrs') <- merge'into't'env (map (second (closeOver . apply subst)) t'binds) (infer'groups sccs)
            return (t'env', t'constrs ++ t'constrs', k'constrs ++ k'constrs')


-- | NOTE: this can stay like this for now
-- infer'many :: [(String, Expression)] -> Analyze ([(String, Type)], [Constraint Type], [Constraint Kind])
-- infer'many bindings = do
--   let indexed = index'bindings bindings
--   let graph = build'graph bindings indexed
--   let solved = stronglyConnComp graph
--   -- ted to mam vyreseny a co musim udelat je
--   -- ze projdu celej ten   list a pro kazdy CyclicSCC [(String, Expression)]
--     -- priradim kazdymu jmenu Forall [] <$> fresh
--     -- pak vlastne provedu posbirani constraintu
--     -- pak je vratim nekam
--   -- pro kazdy AcyclicSCC (String, Expression)
--     -- tady to Expression nezavisi ani samo na sobe, takze neni potreba to zanaset
--     -- jenom to infernu -> posbiram constrainty a type a vratim je nekam vejs
--   infer'groups solved
--     where
--       infer'groups :: [SCC (String, Expression)] -> Analyze ([(String, Type)], [Constraint Type], [Constraint Kind])
--       infer'groups [] = return ([], [], [])
--       infer'groups ((AcyclicSCC bind) : sccs) = do
--         (t'binds, constrs, k'constrs) <- infer'group [bind]
--         -- (k'env, t'env, ali'env) <- ask
--         t'env <- asks type'env
--         (t'binds', constrs', k'constrs') <- merge'into't'env (map (\ (n, t) -> (n, generalize t'env t)) t'binds) $ infer'groups sccs
--         return (t'binds ++ t'binds', constrs ++ constrs', k'constrs ++ k'constrs')

--       infer'groups ((CyclicSCC bindings) : sccs) = do
--         (t'binds, constrs, k'constrs) <- infer'group bindings
--         -- (k'env, t'env, ali'ev) <- ask
--         t'env <- asks type'env
--         (t'binds', constrs', k'constrs') <- merge'into't'env (map (\ (n, t) -> (n, generalize t'env t)) t'binds) $ infer'groups sccs
--         return (t'binds ++ t'binds', constrs ++ constrs', k'constrs ++ k'constrs')


-- | NOTE: this can stay like this for now
infer'group :: [(String, Expression)] -> Analyze ([(String, Type)], [Constraint Type], [Constraint Kind])
infer'group bindings = do
  let names = map fst bindings
      gener name = do ForAll [] <$> (TyVar <$> fresh)
  fresh'vars <- mapM gener names
  merge'into't'env (zip names fresh'vars) $ infer'many' bindings

infer'one :: (String, Expression) -> Analyze ((String, Type), [Constraint Type], [Constraint Kind])
infer'one (name, type') = do
  fresh'name <- fresh
  let fresh'var = ForAll [] (TyVar fresh'name)
  put'in't'env (name, fresh'var) $ infer'one' (name, type')


-- | NOTE: this can stay like this for now
infer'many' :: [(String, Expression)] -> Analyze ([(String, Type)], [Constraint Type], [Constraint Kind])
infer'many' [] = do
  return ([], [], [])
infer'many' ((name, expr) : exprs) = do
  (type', constraints, k'constrs) <- infer expr

  orig'type <- lookup't'env name
  (types, constrs', k'constrs') <- infer'many' exprs
  return ((name, type') : types, (orig'type, type') : constraints ++ constrs', k'constrs ++ k'constrs')


infer'one' :: (String, Expression) -> Analyze ((String, Type), [Constraint Type], [Constraint Kind])
infer'one' (name, expr) = do
  (type', t'constraints, k'constrs) <- infer expr
  orig'type <- lookup't'env name
  return ((name, type'), (orig'type, type') : t'constraints, k'constrs)
