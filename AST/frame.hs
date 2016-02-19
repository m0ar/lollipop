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
        | VDouble Double
        | VBoolean Bool
        | VLam Exp Context
        | VCon ConID [Value] -- list of values to be used as parameters
    deriving Show

type ConID = String

data Env = Env Decs [Context]

type Decs = Map Var Value
type Context = Map Var Exp

data Lit = SLit String
        | ILit Int
        | DLit Double
        | BLit Bool
    deriving Show

addValues :: Value -> Value -> Value
addValues (VInt x) (VInt y) = VInt (x+y)

interpret :: Program -> IO Value
interpret ds =
    do
        let d = addDecs ds M.empty
        let e = Env d [M.empty]
        let value = eval e $ (\(DFunc v vs e) -> e)(head ds)
        return value

addDecs :: [Declaration] -> Decs -> Decs
addDecs [] decs                         = decs
addDecs ((DFunc fname vs expr):ds) decs = addDecs ds (addDecs decs fname (VLam expr M.empty))
    --case expr of
        --ELam _ _ -> addDecsToEnv ds (addToEnv env fname (VLam expr M.empty))
        --_        -> env


addToCxt :: Env -> Var -> Value -> Env
addToCxt e var val = case M.lookup var e of
                Nothing  -> M.insert var val e
                Just val -> M.insert var val (M.delete var e)

eval :: Env -> Exp -> Value
eval env expr = case expr of
        (EApp e1 e2)      -> case eval env e1 of
            (VLam (ELam var expr') cxt) -> eval (addToEnv env var (eval env e2)) expr'
        (EAdd e1 e2)      -> addValues (eval env e1) (eval env e2)
        (ELam var e)      -> eval env e
        (EVar var)        -> fromJust (lookupInEnv env var)
        (ELit (ILit i))   -> (VInt i)
        --(EAdd e1 e2) ->
        --(EApp e1 e2) -> case eval e1 of
            --VExp (ELam v e') -> eval env e'
            --_                ->



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






-- Example types ..
{--data Type = Int | Double | Boolean | Char | IO | Void
    deriving Show --}



-- A function consists of a name (String)
-- and a function body (Body)
-- data Func = Function String Body

-- Arguments
--data Arg = Con Type Arg | Nil

-- Function body consists of
-- a set of variables (Vars) and
-- the statement/command (Stat) to be performed
-- data Body = Body Vars Exp


{-- evalFunc :: Func -> IO ()
evalFunc (Function _ (Body _ e)) = case e of
                EPrint e' -> putStrLn (show e') --}

{-- readExpr :: String -> Maybe Func
readExpr s = Nothing --}


{-- instance Show Func where
    show (Function s b) = "function " ++ s
                                 ++ " : "
                                 ++ "\n"
                                 ++ show b
instance Show Arg where
    show a = case a of
        Nil      -> ""
        Con t a' -> (show t) ++ " -> " ++ show a' --}

{-- instance Show Body where
    show (Body vs c) = " "
                    ++ show vs
                    ++ "= "
                    ++ show c--}
