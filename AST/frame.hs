type Program = [Declaration]

data Declaration = DFun String Exp |
                   DConst String Exp

data Exp = Application Exp Exp |
           Var String |
           SLit String

-- [DConst "main" (Appl (Var "print") (SLit "hello"))]

eval :: Program -> IO ()
eval p = undefined

-- A function consists of a name (String),
-- a set of input arguments (Arg),
-- a return type (Type) and a body (Body)
-- todo add function body
data Func = Function String Arg Type

-- Arguments
data Arg = Con Type Arg | Nil

-- Example types ..
data Type = Int | Double | Boolean | Char
    deriving Show


instance Show Arg where
    show a = case a of
        Nil      -> ""
        Con t a' -> (show t) ++ " -> " ++ show a'

instance Show Func where
    show (Function s a t) = "function " ++ s
                                 ++ " : "
                                 ++ show a
                                 ++ show t
