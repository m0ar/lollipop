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
        let value = eval e $ (\(DFunc v vs e) -> e)(head ds)
        return value

-- addDecsToEnv is a helper function to interpret

-- Adds declarations to the environment
addDecsToEnv :: Env -> [Declaration] -> Env
addDecsToEnv env []     = M.insert "print" (VFun (\(VString s) -> (VIO s))) M.empty
addDecsToEnv env (d:ds) = uncurry M.insert (makeBinding d env) e'
    where
        e' = addDecsToEnv env ds

-- makeBinding is a helper function to addDecsToEnv
-- Makes bindings from declarations to environment
makeBinding :: Declaration -> Env -> (Var, Value)
makeBinding (DConstr id val) env  = (id, val)
makeBinding (DFunc name vs e) env = (name, eval env (addLams vs e))
        where
            addLams [] e     = e
            addLams (v:vs) e = ELam v (addLams vs e)



equals :: Value -> Value -> Bool
equals (VInt x) (VInt y) = x == y

-- evaluation of an expression in an environment
eval :: Env -> Exp -> Value
eval env expr = case expr of
        ELetIn var e1 e2         -> eval env' e2
            where env' = addToEnv env var (eval env' e1)
        EConstr cid              -> lookupInEnv env cid
        ECase e ps               -> eval env' e'
            where (VConstr cid vals)    = eval env e
                  (Constr cid' vars e') = findPattern ps cid
                  env'             = addManyToEnv env vars vals
        EApp e1 e2               -> case (eval env e1) of
             VFun v1                -> v1 v2
                where v2 = eval env e2
             _                      -> error "NOT FUNCTION!!!!"
        ELam var e               -> VFun f
            where f v = eval (addToEnv env var v) e
        EVar var                 -> (lookupInEnv env var)
        ELit (ILit i)            -> VInt i
        ELit (SLit s)            -> VString s
        EBinOp op e1 e2          -> case op of
            Add                     -> (\(VInt x) (VInt y) -> (VInt (x+y)))
                                            (eval env e1) (eval env e2)
            Sub                     -> (\(VInt x) (VInt y) -> (VInt (x-y)))
                                            (eval env e1) (eval env e2)
            Mul                     -> (\(VInt x) (VInt y) -> (VInt (x*y)))
                                            (eval env e1) (eval env e2)


-- finds pattern based on constructor ID
findPattern :: [Pattern] -> ConstrID -> Pattern
findPattern [] _                          = error "could not find pattern"
findPattern (p@(Constr cid vs expr):ps) cid'
                            | cid == cid' = p
                            | otherwise   = findPattern ps cid'
