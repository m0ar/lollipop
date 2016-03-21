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
       | ECase Exp [(Pattern, Exp)]
       | ELetIn Var Exp Exp  -- let var = exp in exp
       | ETup2 Exp Exp
       | ETup3 Exp Exp Exp

data Op = Add | Sub | Mul | Div | Bind


data Pattern = PConstr ConstrID [Var]
            | PLit Lit
            | PWild
            | PVar Var
            -- | List LPattern
            -- | Tup2 Pattern Pattern
            -- | Tup3 Pattern Pattern Pattern

-- data LPattern = LP LEntry LPattern | Nil

-- data LEntry = Var Var | Lit Lit | LWild | Empty

type Var = String

-- A list of variables to be used in function bodies
type Vars = [Var]

data Value = VIO (IO Value) -- void IO
        | VString String -- TODO Remove
        | VLit Lit
        | VConstr ConstrID [Value] -- list of values to be used as parameters
        | VFun (Value -> Value)

type ConstrID = String

data Lit = ILit Int
        | DLit Double
        | CLit Char
        | SLit [Char]
    deriving Eq

instance Show Op where
    show op = case op of
        Add  -> "#add"
        Sub  -> "#sub"
        Mul  -> "#mul"
        Div  -> "#div"
        Bind -> "#bind"

instance Show Lit where
    show lit = case lit of
        SLit s -> s
        ILit i -> show i
        DLit d -> show d
        CLit c -> [c]

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
        PConstr cid vs -> cid ++ " "  ++ (concatMap show vs) ++ " = "
        PWild          -> "_ -> "
        PVar v    -> v ++ " -> "

instance Show Value where
    show v = case v of
        (VLit lit)   -> show lit
        (VIO v)      -> "IO!!!!"
        (VFun f)     -> "gotta function"
        (VConstr cid vs) -> cid ++ " " ++ concat (Prelude.map show vs)
