module Frame where

import Data.Maybe
import Text.Parsec
import Environment
import DataTypes
import Data.Map
import qualified Data.Map as M

interpret :: Program -> IO Value
interpret ds =
    do
        let e = addDecsToEnv e ds
        let e1 = M.insert "cons" (VFun (\v1 -> VFun (\v2 -> VCon "cons" [v1,v2]))) e
        let e2 = M.insert "nil" (VCon "nil" []) e1
        let value = eval e2 $ (\(DFunc v vs e2) -> e2)(head ds)
        return value

-- addDecsToEnv is a helper function to interpret

-- Adds declarations to the environment
addDecsToEnv :: Env -> [Declaration] -> Env
addDecsToEnv env []     = M.empty
-- addDecsToEnv env ((DConstr cid val):ds) = addDecsToEnv (M.insert cid val env) ds
addDecsToEnv env (d:ds) = uncurry M.insert (makeBinding d env) e'
    where
        e' = addDecsToEnv env ds

-- makeBinding is a helper function to addDecsToEnv

-- Makes bindings from declarations to environment
makeBinding :: Declaration -> Env -> (Var, Value)
makeBinding (DFunc name vs e) env = (name, eval env (addLams vs e))
    where
        addLams [] e     = e
        addLams (v:vs) e = ELam v (addLams vs e)


-- evaluation of an expression in an environment
eval :: Env -> Exp -> Value
eval env expr = case expr of
        EGuard [] e              -> eval env e
        EGuard ((e1,e2):ts) e      -> case (eval env e1) of
            VBoolean True     -> eval env e2
            VBoolean False    -> eval env (EGuard ts e)
            _                 -> error " not boolean statement"
        EWhere var e1 e2         -> eval env' e1
            where env' = addToEnv env var (eval env e2)
        ELetIn var e1 e2           -> eval env' e2
            where env' = addToEnv env var (eval env e1)
        ECon cid                 -> lookupInEnv env cid
        -- ECon cid []              -> VString cid
        -- ECon cid (e:es)             -> eval env e
        -- ECon cid es              -> VCon cid $ Prelude.map (\e -> eval env e) es
        ECase e ps               -> eval env' e'
            where (VCon cid vals)  = eval env e
                  (cid', vars, e') = findPattern ps cid
                  env'             = addManyToEnv env vars vals
        EIf e1 e2 e3             -> case (eval env e1) of
             VBoolean True     -> eval env e2
             VBoolean False    -> eval env e3
             _                 -> error " not boolean statement"
        EPrint e                 -> VIO (show $ eval env e)
        EApp e1 e2               -> case (eval env e1) of
             VFun v1           -> v1 v2
                where v2 = eval env e2
             _                 -> error "NOT FUNCTION!!!!"
        EAdd e1 e2               -> evalAddition (eval env e1) (eval env e2)
        ELam var e               -> (VFun f)
            where f v = eval (addToEnv env var v) e
        EVar var                 -> (lookupInEnv env var)
        ELit (ILit i)            -> (VInt i)
        ELit (SLit s)            -> (VString s)
        ELit (BLit b)            -> (VBoolean b)

-- concrete evaluation of an addition
evalAddition :: Value -> Value -> Value
evalAddition (VInt x) (VInt y) = VInt (x+y)

-- finds pattern based on constructor ID
findPattern :: [Pattern] -> ConID -> Pattern
findPattern [] _                          = error "could not find pattern"
findPattern (p@(cid, vs, expr):ps) cid'
                            | cid == cid' = p
                            | otherwise   = findPattern ps cid'
