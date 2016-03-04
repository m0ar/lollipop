module DataTypes where


type Program = [Declaration]

-- type Declaration = (Var, Vars, [Definition])

-- data Definition = DFunc Vars Exp
data Declaration = 
         DFunc Var Vars Exp
       | DConstr ConstrID Vars --Value

data Exp = EApp Exp Exp
       | EVar Var
       | ELit Lit
       -- | EList List
       | EAdd Exp Exp
       | EMult Exp Exp
       | EPrint Exp
       | ELam Var Exp
       | EIf Exp Exp Exp
       | EConstr ConstrID --[Exp]
       | ECase Exp [Pattern]
       | ELetIn Var Exp Exp  -- let var = exp in exp
       | EWhere Var Exp Exp
       | EGuard [(Exp, Exp)] Exp

type Pattern = (ConstrID, [Var], Exp)
type Var = String

-- A list of variables to be used in function bodies
type Vars = [Var]

-- A list of listerals
-- data List = Cons Lit List | Nil
--     deriving Show

data Value = VInt Int
        | VIO String
        | VString String
        | VDouble Double
        | VBoolean Bool
        | VConstr ConstrID [Value] -- list of values to be used as parameters
        | VFun (Value -> Value)

type ConstrID = String

data Lit = SLit String
        | ILit Int
        | DLit Double
        | BLit Bool
    deriving Show

-- Show functions --
instance Show Exp where
    show e = case e of
        EApp e1 e2         -> ""
        EVar s             -> s
        ELit l             -> show l
        EConstr cid           -> show cid
        EAdd e1 e2         -> show e1 ++ " + " ++ show e2
        EMult e1 e2        -> show e1 ++ " * " ++ show e2

instance Show Value where
    show v = case v of
        (VInt x)    -> show x
        (VString s) -> s
        (VIO s)     -> s
        (VFun f)    -> "gotta function"
        (VConstr cid vs) -> cid ++ " " ++ concat (Prelude.map show vs)
