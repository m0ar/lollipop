{-# LANGUAGE DeriveDataTypeable #-}
module AST.DataTypes where

import ErrM
import Control.Exception
import Data.Typeable

data LoliException = NoSuchFile | SyntaxError | LinearException
    deriving (Show, Typeable)
instance Exception LoliException

type Program = [Declaration]


data Declaration =
         DFunc Var Type Vars Exp
       | DConstr ConstrID Value -- change Value to something else later


data Exp = EApp Exp Exp
       | EVar Var
       | EiVar Var
       | ELit Lit
       | EUnOp Op Exp
       | EBinOp Op Exp Exp
       | ELam Var Exp
       | EConstr ConstrID
       | EiConstr ConstrID
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
        | VLit Lit
        | VConstr ConstrID [Value] -- list of values to be used as parameters
        | VFun (Value -> Value)

type ConstrID = String

data Lit = ILit Int
        | DLit Double
        | CLit Char
        | SLit [Char]
    deriving Eq

instance Eq Value where
    (==) (VLit (ILit x)) (VLit (ILit y)) = x == y
    (==) (VLit (DLit x)) (VLit (DLit y)) = x == y
    (==) (VLit (SLit x)) (VLit (SLit y)) = x == y
    (==) (VLit (CLit x)) (VLit (CLit y)) = x == y
    (==) _                _              = False

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
    show (DFunc var tDecls vars e) = "function: " ++ var ++ "\n " ++ (show e)

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
        (VConstr cid vs) -> case cid of
            "(,)"   -> "(" ++ (show (vs !! 0)) ++ ", " ++ (show (vs !! 1)) ++ ")"
            "(,,)"  -> "(" ++ (show (vs !! 0)) ++ ", " ++ (show (vs !! 1)) ++ ", " ++ (show (vs !! 2)) ++ ")"
            "Nil"   -> "[]"
            "Cons"  -> "[" ++ (AST.DataTypes.showList vs) ++ "]"
            _       -> cid ++ " " ++ concat (Prelude.map show vs)

showList :: [Value] -> String
showList [v, (VConstr "Nil"  [])] = show v
showList [v, (VConstr "Cons" vs)] = (show v) ++ ", " ++ AST.DataTypes.showList vs


-- Types 
data Type =
    TVar Var
    | TConstr String
    | TFun Type Type
    | TApp Type Type
    deriving (Eq, Ord)