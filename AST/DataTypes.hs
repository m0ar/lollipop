module AST.DataTypes where


type Program = [Declaration]


data Declaration = DFunc Var Vars Exp
       | DConstr ConstrID Value -- change Value to something else later


data Exp = EApp Exp Exp
       | EVar Var
       | ELit Lit
       | EUnOp Op Exp
       | EBinOp Op Exp Exp
       | ELam Var Exp
       | EConstr ConstrID
       | ECase Exp [(Pattern, Exp)]
       | ELetIn Var Exp Exp  -- let var = exp in exp
       | EListComp Exp [(Var, Value)] Exp

data Op = Cons | Concat | Add | Sub | Mul | Div
        | Gt | Eq | Or | Not | Pow | Bind | Then

data Pattern = PConstr ConstrID [Pattern]
            | PLit Lit
            | PWild
            | PVar Var

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
        | SLit String
    deriving Eq


instance Num Lit where
    (+) (ILit x) (ILit y) = ILit (x+y)
    (+) (DLit x) (DLit y) = DLit (x+y)
    (+) (ILit x) (DLit y) = DLit ((fromIntegral x)+y)
    (+) (DLit x) (ILit y) = DLit (x+(fromIntegral y))
    (*) (ILit x) (ILit y) = ILit (x*y)
    (*) (DLit x) (DLit y) = DLit (x*y)
    (*) (ILit x) (DLit y) = DLit ((fromIntegral x)*y)
    (*) (DLit x) (ILit y) = DLit (x*(fromIntegral y))

instance Show Op where
    show op = case op of
        Concat -> "#concat"
        Cons   -> "#cons"
        Gt     -> "#gt"
        Eq     -> "#eq"
        Not    -> "#not"
        Or     -> "#or"
        Add    -> "#add"
        Mul    -> "#mul"
        Pow    -> "#pow"
        Div    -> "#div"
        Bind   -> "#bind"
        Then   -> "#then"

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
        EApp e1 e2         -> show e1 ++ " " ++ show e2
        EVar s             -> s
        ELit l             -> show l
        EConstr cid        -> show cid
        EUnOp op e         -> case op of
            Not -> "!" ++ show e
            _   -> "Unary operator: '" ++ show op ++ "'"
        EBinOp op e1 e2    -> case op of
            Add  -> disp "+"
            Sub  -> disp "-"
            Mul  -> disp "*"
            Div  -> disp "/"
            Bind -> disp "<-"
            Then -> disp ">>"
            Gt   -> disp ">"
            Eq   -> disp "=="
            Or   -> disp "||"
            Pow  -> disp "^"
            _    -> show e1 ++ " binary operator: '" ++ show op ++ "' " ++ show e2
            where 
                disp op = show e1 ++ " " ++ op ++ " " ++ show e2 
        ELam v e           -> "\\" ++ v ++ " -> " ++ show e
        ECase e ps         -> "case " ++ show e ++ " of \n" ++ (concatMap show ps)
        ELetIn v e1 e2     -> "let " ++ v ++ " = " ++ show e1 ++ " in \n   " ++ show e2

instance Show Pattern where
    show p = case p of
        PConstr cid vs -> cid ++ " "  ++ (concatMap show vs) ++ " = "
        PLit l         -> show l
        PWild          -> "_ -> "
        PVar v    -> v ++ " -> "

instance Show Value where
    show v = case v of
        (VLit lit)   -> show lit
        (VIO v)      -> "IO!!!!"
        (VFun f)     -> "gotta function"
        (VConstr cid vs) -> cid ++ " " ++ concat (Prelude.map show vs)
