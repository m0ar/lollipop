module AST.Environment where

import Data.Map
import qualified Data.Map as M
import AST.DataTypes

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
lookupInEnv env var = case val of
        Nothing -> error $ "variable: " ++ var ++ " not found in environment: \n" ++ show env
        Just v  -> v
    where val = M.lookup var env
