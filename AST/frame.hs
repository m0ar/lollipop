type Program = [Declaration]

data Declaration = DFun String Pattern |
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
    deriving Show

-- Arguments
data Arg = Con Type Arg | Nil
    deriving Show

-- Example types ..
data Type = Int | Double | Boolean | Char
    deriving Show


-- A quick parse-function for our function type
parseF :: Func -> String
parseF (Function s a t) = "function " ++ s
                             ++ " : "
                             ++ parseA a
                             ++ (show t)
    where
        parseA a = case a of
            Nil         -> ""
            (Con t a')  -> (show t) ++ " -> " ++ parseA a'
