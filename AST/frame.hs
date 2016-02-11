type Program = [Declaration]

data Declaration = DFun String Pattern |
                   DConst String Exp

data Exp = Application Exp Exp |
           Var String |
           SLit String

-- [DConst "main" (Appl (Var "print") (SLit "hello"))]

eval :: Program -> IO ()
eval p = undefined