module DataTypes where


type Program = [Declaration]


data Declaration =
         DFunc Var Vars Exp
       | DConstr ConstrID Value -- change Value to something else later

data Exp = EApp Exp Exp
       | EVar Var
       | ELit Lit
       | EAdd Exp Exp
       | EMult Exp Exp
       | EPrint Exp
       | ELam Var Exp
       | EIf Exp Exp Exp
       | EConstr ConstrID
       | ECase Exp [Pattern]
       | ELetIn Var Exp Exp  -- let var = exp in exp
       | EWhere Var Exp Exp
       | EPattern Exp [Pattern]

data Pattern = Constr ConstrID [Var] Exp
            | Simple SimpleValue Exp
            | Variable Vars

type Var = String

-- A list of variables to be used in function bodies
type Vars = [Var]

data SimpleValue = SimpleInt Int
        | SimpleChar Char
        | SimpleBool Bool

data Value = VInt Int
        | VIO String
        | VString String
        | VChar Char
        | VDouble Double
        | VBoolean Bool
        | VConstr ConstrID [Value] -- list of values to be used as parameters
        | VFun (Value -> Value)

type ConstrID = String

data Lit = SLit String
        | ILit Int
        | DLit Double
        | BLit Bool
        | CLit Char
    deriving Show

-- Show functions --
instance Show Exp where
    show e = case e of
        EApp e1 e2         -> ""
        EVar s             -> s
        ELit l             -> show l
        EConstr cid        -> show cid
        EAdd e1 e2         -> show e1 ++ " + " ++ show e2
        EMult e1 e2        -> show e1 ++ " * " ++ show e2

instance Show Value where
    show v = case v of
        (VInt x)    -> show x
        (VString s) -> s
        (VIO s)     -> s
        (VFun f)    -> "gotta function"
        (VConstr cid vs) -> cid ++ " " ++ concat (Prelude.map show vs)
