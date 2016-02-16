type Program = [Declaration]

data Declaration = DFun String Exp
                    | DConst String Exp

data Exp = Application Exp Exp
           | Var String
           | SLit String
           | Add Exp Exp
           | Mult Exp Exp
   deriving Show

-- [DConst "main" (Appl (Var "print") (SLit "hello"))]

eval :: Program -> IO ()
eval p = undefined

-- A simple datatype for modelling commands, which can
-- either be an assign, declaration, a print or a simple
-- expression
data Com = Assign String Exp
            | Declare String Exp Com
            | Print Exp
            | Exp Exp
    deriving Show

-- A function consists of a name (String),
-- a set of input arguments (Arg),
-- a return type (Type) and a body (Body)
-- todo add function body
data Func = Function String Arg Type Body

-- Arguments
data Arg = Con Type Arg | ANil

-- Function body consists of the function name (String),
-- a set of variables (Vars) and
-- the command (Com) to be performed
data Body = Body String Vars Com

-- A set of variables to be used in function bodies
data Vars = Variable String Vars | VNil

-- Example types ..
data Type = Int | Double | Boolean | Char
    deriving Show


-- Show functions --

instance Show Func where
    show (Function s a t b) = "function " ++ s
                                 ++ " : "
                                 ++ show a
                                 ++ show t
                                 ++ "\n"
                                 ++ show b
instance Show Arg where
    show a = case a of
        ANil     -> ""
        Con t a' -> (show t) ++ " -> " ++ show a'

instance Show Body where
    show (Body s vs c) = s ++ " "
                        ++ show vs
                        ++ "= "
                        ++ show c

instance Show Vars where
    show var = case var of
        VNil            -> ""
        (Variable s vs) -> s ++ " " ++ show vs
