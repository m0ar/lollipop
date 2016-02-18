import Data.Maybe
import Text.Parsec

type Program = [Declaration]

data Declaration = DFun Func
                    | DConst String Exp

data Exp = Application Exp Exp
           | Var String
           | SLit String
           | Add Exp Exp
           | Mult Exp Exp
           | Print Exp

instance Show Exp where
    show e = case e of
        Application e1 e2 -> ""
        Var s             -> s
        SLit s            -> s
        Add e1 e2         -> show e1 ++ " + " ++ show e2
        Mult e1 e2        -> show e1 ++ " * " ++ show e2

-- [DConst "main" (Appl (Var "print") (SLit "hello"))]

eval :: Program -> IO ()
eval p = undefined

evalFunc :: Func -> IO ()
evalFunc (Function _ _ _ (Body _ e)) = case e of
            Print e'         -> putStrLn (show e')

-- A function consists of a name (String),
-- a set of input arguments (Arg),
-- a return type (Type) and a body (Body)
-- todo add function body
data Func = Function String Arg Type Body

-- Arguments
data Arg = Con Type Arg | ANil

-- Function body consists of
-- a set of variables (Vars) and
-- the statement/command (Stat) to be performed
data Body = Body Vars Exp

-- A set of variables to be used in function bodies
data Vars = Variable String Vars | VNil

-- Example types ..
data Type = Int | Double | Boolean | Char | IO | Void
    deriving Show



readExpr :: String -> Maybe Func
readExpr s = Nothing


-- Show functions --

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
        ANil     -> ""
        Con t a' -> (show t) ++ " -> " ++ show a'

instance Show Body where
    show (Body vs c) = " "
                        ++ show vs
                        ++ "= "
                        ++ show c

instance Show Vars where
    show var = case var of
        VNil            -> ""
        (Variable s vs) -> s ++ " " ++ show vs
