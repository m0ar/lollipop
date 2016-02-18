import Data.Maybe
import Text.Parsec

type Program = [Declaration]

data Declaration = DFun Func
                    | DConst String Exp

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

-- A function consists of a name (String),
-- a set of input arguments (Arg),
-- a return type (Type) and a body (Body)
-- todo add function body
data Func = Function String Arg Type Body

-- Arguments
data Arg = Con Type Arg | Nil

-- Function body consists of
-- a set of variables (Vars) and
-- the statement/command (Stat) to be performed
data Body = Body Vars Exp

-- Example types ..
data Type = Int | Double | Boolean | Char | IO | Void
    deriving Show

data Value =

data Lit = SLit String
            | ILit Int
            | DLit Double
            | BLit Bool
    deriving Show

-- [DConst "main" (Appl (Var "print") (SLit "hello"))]

eval :: Program -> IO ()
eval p = undefined

evalFunc :: Func -> IO ()
evalFunc (Function _ _ _ (Body _ e)) = case e of
            EPrint e'         -> putStrLn (show e')

readExpr :: String -> Maybe Func
readExpr s = Nothing


-- Show functions --


instance Show Exp where
    show e = case e of
        EApp e1 e2 -> ""
        EVar s             -> s
        ELit l             -> show l
        EAdd e1 e2         -> show e1 ++ " + " ++ show e2
        EMult e1 e2        -> show e1 ++ " * " ++ show e2

instance Show Func where
    show (Function e a t b) = "function " ++ e
                                 ++ " : "
                                 ++ show a
                                 ++ show t
                                 ++ "\n"
                                 ++ e
                                 ++ show b
instance Show Arg where
    show a = case a of
        Nil      -> ""
        Con t a' -> (show t) ++ " -> " ++ show a'

instance Show Body where
    show (Body vs c) = " "
                    ++ show vs
                    ++ "= "
                    ++ show c
