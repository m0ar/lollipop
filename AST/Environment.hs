module AST.Environment where

import Data.Map
import qualified Data.Map as M
import AST.DataTypes
import Control.Exception

type Env = Map Var Value


-- Adds a variable, value mapping to environment
addToEnv :: Env -> Var -> Value -> Env
addToEnv env var val = case M.lookup var env of
                Nothing  -> M.insert var val env
                Just val -> M.insert var val (M.delete var env)

-- Adds many variable, value mappings to environment.
-- Used in case and pattern matching
addManyToEnv :: Env -> [Var] -> [Value] -> Env
addManyToEnv env   []        []      = env
addManyToEnv env   []        _       = error "variables and values not same length"
addManyToEnv env   _        []       = error "variables and values not same length"
addManyToEnv env (v1:vars) (v2:vals) = addManyToEnv (addToEnv env v1 v2) vars vals

lookupInEnv :: Env -> Var -> Value
lookupInEnv env var = case M.lookup var env of
        Nothing -> case M.lookup ("!" ++ var) env of
            Nothing -> VConstr "Undefined" []
            Just v  -> throw LinearException -- if variable found as used linear
        Just v  -> v

-- consumes, and unbinds a linear variable
-- once used the name of the variable starts
-- with an !
consumeLinear :: Env -> Var -> Env
consumeLinear e v = M.insert ("!" ++ v) (VConstr "" []) e'
    where e' = M.delete v e
