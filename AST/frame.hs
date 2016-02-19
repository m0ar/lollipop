import Data.Maybe
import Text.Parsec
import Data.Map
import qualified Data.Map as M

type Program = [Declaration]


data Declaration = DFunc Var Vars Exp

data Exp = EApp Exp Exp
       | EVar Var
       | ELit Lit
       | EAdd Exp Exp
       | EMult Exp Exp
       | EPrint Exp
       | ELam Var Exp


type Var = String

-- A list of variables to be used in function bodies
type Vars = [Var]


data Value = VInt Int
        | VIO String
        | VString String
        | VDouble Double
        | VBoolean Bool
        | VLam Exp
        | VCon ConID [Value] -- list of values to be used as parameters

instance Show Value where
    show v = case v of
        (VInt x)    -> show x
        (VString s) -> s
        (VIO s)     -> s

type ConID = String

type Env = Map Var Value

data Lit = SLit String
        | ILit Int
        | DLit Double
        | BLit Bool
    deriving Show

addValues :: Value -> Value -> Value
addValues (VInt x) (VInt y) = VInt (x+y)

-- test-code :
-- let p = [(DFunc "main" [] (EApp (ELam "x" (EAdd (EVar "x") (ELit (ILit 4)))) (ELit (ILit 6))))]
-- interpret p
-- let p = [(DFunc "main" [] (EPrint (ELit (SLit "hi"))))]

interpret :: Program -> IO Value
interpret ds =
    do
        let e = addDecsToEnv ds M.empty
        let value = eval e $ (\(DFunc v vs e) -> e)(head ds) in
            return value

addDecsToEnv :: [Declaration] -> Env -> Env
addDecsToEnv [] env                         = env
addDecsToEnv ((DFunc fname vs expr):ds) env = addDecsToEnv ds (addToEnv env fname (VLam expr))
    --case expr of
        --ELam _ _ -> addDecsToEnv ds (addToEnv env fname (VLam expr M.empty))
        --_        -> env


addToEnv :: Env -> Var -> Value -> Env
addToEnv env var val = case M.lookup var env of
                Nothing  -> M.insert var val env
                Just val -> M.insert var val (M.delete var env)

eval :: Env -> Exp -> Value
eval env expr = case expr of
        (EPrint e)                 -> VIO (show $ eval env e)
        (EApp (ELam var expr') e2) -> let env2 = (addToEnv env var (eval env e2)) in eval env2 expr'
        (EAdd e1 e2)               -> addValues (eval env e1) (eval env e2)
        (ELam var e)               -> eval env e
        (EVar var)                 -> case lookupInEnv env var of
            Nothing -> error $ "variable: " ++ var ++ " not found in environment: \n" ++ show env
            Just v  -> v
        (ELit (ILit i))            -> (VInt i)
        (ELit (SLit s))            -> (VString s)



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
