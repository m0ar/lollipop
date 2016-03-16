module DataTypes where


type Program = [Declaration]


data Declaration =
         DFunc Var Vars Exp
       | DConstr ConstrID Value -- change Value to something else later


data Exp = EApp Exp Exp
       | EVar Var
       | ELit Lit
       | EBinOp Op Exp Exp
       | ELam Var Exp
       | EConstr ConstrID
       | ECase Exp [Pattern]
       | ELetIn Var Exp Exp  -- let var = exp in exp
       | ETup2 Exp Exp
       | ETup3 Exp Exp Exp

data Op = Add | Sub | Mul | Div | Bind
    deriving Show


data Pattern = Constr ConstrID [Var]
            | Literal Lit
            -- | Simple Lit
            | Wild
            | Variable Var
            -- | Sim Pat -- A better version of Simple
            -- | List LPattern
            -- | Tup2 Pattern Pattern
            -- | Tup3 Pattern Pattern Pattern

data Pat = PLit Lit | PWild | PVar Var
    deriving Show

data LPattern = LP LEntry LPattern | Nil

data LEntry = Var Var | Lit Lit | LWild | Empty

type Var = String

-- A list of variables to be used in function bodies
type Vars = [Var]

data Value = VInt Int
        | VIO (IO Value) -- void IO
        | VString String
        | VChar Char
        | VDouble Double
        | VConstr ConstrID [Value] -- list of values to be used as parameters
        | VFun (Value -> Value)
        | VTup2 Value Value
        | VTup3 Value Value Value

type ConstrID = String

data Lit = SLit String
        | ILit Int
        | DLit Double
        | CLit Char
    deriving Show

instance Show Declaration where
    show (DFunc var vars e) = "function: " ++ var ++ "\n " ++ (show e)

-- Show functions --
instance Show Exp where
    show e = case e of
        EApp e1 e2         -> ""
        EVar s             -> s
        ELit l             -> show l
        EConstr cid        -> show cid
        EBinOp op e1 e2    -> case op of
            Add -> (show e1) ++ " + " ++ (show e2)
        ELam v e           -> "(\'" ++ v ++ " -> " ++ (show e)
        ECase e ps         -> "case " ++ (show e) ++ " of \n" ++ (concatMap show ps)

instance Show Pattern where
    show p = case p of
        Constr cid vs -> cid ++ " "  ++ (concatMap show vs) ++ " = "
        -- Simple lit    -> (show lit) ++ " -> "
        Wild          -> "_ -> "
        Variable v    -> v ++ " -> "

instance Show Value where
    show v = case v of
        (VInt x)     -> show x
        (VString s)  -> s
        (VIO v)      -> "IO!!!!"
        (VFun f)     -> "gotta function"
        (VConstr cid vs) -> cid ++ " " ++ concat (Prelude.map show vs)
        (VTup2 v1 v2)    -> "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
        (VTup3 v1 v2 v3) -> "(" ++ show v1 ++ ", " ++ show v2 ++ ", " ++ show v3 ++ ")"
