module Frame where

import Data.Maybe
import Text.Parsec
import Data.Map
import qualified Data.Map as M

type Program = [Declaration]

data Declaration = DFunc Var Vars Exp

data Exp = EApp Exp Exp
       | EVar Var
       | ELit Lit
       | EList List
       | EAdd Exp Exp
       | EMult Exp Exp
       | EPrint Exp
       | ELam Var Exp
       | EIf Exp Exp Exp
       | ECon ConID [Exp]
       | ECase Exp [Pattern]
       | ELet Var Exp Exp  -- let var = exp in exp

type Pattern = (ConID, [Var], Exp)
type Var = String

-- A list of variables to be used in function bodies
type Vars = [Var]

-- A list of listerals
data List = Cons Lit List | Nil
    deriving Show

data Value = VInt Int
        | VIO String
        | VString String
        | VDouble Double
        | VBoolean Bool
        | VCon ConID [Value] -- list of values to be used as parameters
        | VFun (Value -> Value)

instance Show Value where
    show v = case v of
        (VInt x)    -> show x
        (VString s) -> s
        (VIO s)     -> s
        (VFun f)    -> "gotta function"
        (VCon cid vs) -> cid ++ " " ++ concat (Prelude.map show vs)

type ConID = String

type Env = Map Var Value

data Lit = SLit String
        | ILit Int
        | DLit Double
        | BLit Bool
    deriving Show

interpret :: Program -> IO Value
interpret ds =
    do
        let e = addDecsToEnv e ds
        let value = eval e $ (\(DFunc v vs e) -> e)(head ds)
        return value

-- Adds declarations to the environment
addDecsToEnv :: Env -> [Declaration] -> Env
addDecsToEnv env []     = M.empty
addDecsToEnv env (d:ds) = uncurry M.insert (makeBinding d env) e'
    where
        e' = addDecsToEnv env ds

-- addDecToEnv :: Env -> Declaration -> Env
-- addDecToEnv env (DFunc fname [] expr)   = M.insert fname (eval env expr) env
-- addDecToEnv env (DFunc fname vars expr) = M.insert fname (fst (createContext vars expr)) env

-- createContext :: [Var] -> Exp -> (Value, Env)
-- createContext (v:[]) expr = (value, (addToEnv M.empty v value))
--     where value = (VFun (\val -> eval (M.insert v val M.empty) expr))
-- createContext (v:vs) expr = (value, (addToEnv (snd (valEnv)) v value))
--    where valEnv = createContext vs expr
--          value = (VFun (\val -> (fst valEnv)))

makeBinding :: Declaration -> Env -> (Var, Value)
makeBinding (DFunc name vs e) env = (name, eval env (addLams vs e))
    where
        addLams [] e     = e
        addLams (v:vs) e = ELam v (addLams vs e)

addToEnv :: Env -> Var -> Value -> Env
addToEnv env var val = case M.lookup var env of
                Nothing  -> M.insert var val env
                Just val -> M.insert var val (M.delete var env)

addManyToEnv :: Env -> [Var] -> [Value] -> Env
addManyToEnv env   []        []      = env
addManyToEnv env   []        _       = error "variables and variabels not same length"
addManyToEnv env   _        []       = error "variables and variabels not same length"
addManyToEnv env (v1:vars) (v2:vals) = addManyToEnv (addToEnv env v1 v2) vars vals

eval :: Env -> Exp -> Value
eval env expr = case expr of
        ELet var e1 e2           -> eval env' e2
            where env' = addToEnv env var (eval env e1)
        ECon cid es              -> VCon cid $ Prelude.map (\e -> eval env e) es
        ECase e ps               -> eval env' e'
            where (VCon cid vals)  = eval env e
                  (cid', vars, e') = findPattern ps cid
                  env'             = addManyToEnv env vars vals
        EIf e1 e2 e3             -> case (eval env e1) of
             VBoolean True     -> eval env e2
             VBoolean False    -> eval env e3
             _                   -> error " not boolean statement"
        EPrint e                 -> VIO (show $ eval env e)
        EApp e1 e2               -> case (eval env e1) of
             VFun v1           -> v1 v2
                where v2 = eval env e2
             _                 -> error "NOT FUNCTION!!!!"
        EAdd e1 e2               -> addValues (eval env e1) (eval env e2)
        ELam var e               -> (VFun f)
            where f v = eval (addToEnv env var v) e
        EVar var                 -> case lookupInEnv env var of
            Nothing -> error $ "variable: " ++ var ++ " not found in environment: \n" ++ show env
            Just v  -> v
        ELit (ILit i)            -> (VInt i)
        ELit (SLit s)            -> (VString s)
        ELit (BLit b)            -> (VBoolean b)

addValues :: Value -> Value -> Value
addValues (VInt x) (VInt y) = VInt (x+y)

findPattern :: [Pattern] -> ConID -> Pattern
findPattern [] _                          = error "could not find pattern"
findPattern (p@(cid, vs, expr):ps) cid'
                            | cid == cid' = p
                            | otherwise   = findPattern ps cid'

lookupInEnv :: Env -> Var -> Maybe Value
lookupInEnv e var = M.lookup var e

-- Show functions --
instance Show Exp where
    show e = case e of
        EApp e1 e2         -> ""
        EVar s             -> s
        ELit l             -> show l
        EAdd e1 e2         -> show e1 ++ " + " ++ show e2
        EMult e1 e2        -> show e1 ++ " * " ++ show e2
