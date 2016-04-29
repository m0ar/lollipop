module AST.Environment where

import Data.Map
import qualified Data.Map as M
import AST.DataTypes
import Control.Exception

type Env = Map Var Value


-- Adds a variable, value mapping to environment
addToEnv :: Env -> Var -> Value -> Env
addToEnv env var val = case M.lookup var env of
                Nothing  -> M.insert var val env
                Just val -> M.insert var val (M.delete var env)

-- Adds many variable, value mappings to environment.
-- Used in case and pattern matching
addManyToEnv :: Env -> [Var] -> [Value] -> Env
addManyToEnv env   []        []      = env
addManyToEnv env   []        _       = error "variables and values not same length"
addManyToEnv env   _        []       = error "variables and values not same length"
addManyToEnv env (v1:vars) (v2:vals) = addManyToEnv (addToEnv env v1 v2) vars vals

lookupInEnv :: Env -> Var -> Value
lookupInEnv env var = case M.lookup var env of
        Nothing -> case M.lookup ("!" ++ var) env of
            Nothing -> VConstr "Undefined" []
            Just v  -> throw LinearException -- if variable found as used linear
        Just v  -> v

-- consumes, and unbinds a linear variable
-- once used the name of the variable starts
-- with an !
consumeLinear :: Env -> Var -> Env
consumeLinear e v = M.insert ("!" ++ v) (VConstr "" []) e'
    where e' = M.delete v e

startEnvironment :: [(String, Value, Type)]
startEnvironment = [
                (    "print"
                    ,VFun $ \(VLit (SLit cs)) -> VIO $ vPrint cs
                    ,TFun (TApp (TConstr "[]") (TConstr "Char")) (TConstr "IO")
                ),
                (    "readLine"
                    ,VIO $ fmap (VLit . SLit) readLn
                    ,TApp (TConstr "IO") (TApp (TConstr "[]") (TConstr "Char"))
                ),
                (    "#concat"
                    ,VFun $ \v1 -> VFun $ \v2 -> vConcat v1 v2
                    ,TFun (TApp (TConstr "[]") 
                                (TApp (TConstr "[]") a))
                          (TApp (TConstr "[]") a)
                ),
                (    "#cons"
                    ,VFun $ \v -> VFun $ \(VConstr cid vs) -> (VConstr "Cons" [v, (VConstr cid vs)])
                    ,TFun a (TApp (TConstr "[]") a)
                ),
                (    "#add"
                    ,VFun $ \(VLit x) -> VFun $ \(VLit y) -> VLit $ x+y
                    ,undefined
                ),
                (    "#pow"
                    ,VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ DLit $ (fromIntegral x) ^^ y
                    ,undefined
                ),
                (    "#mul"
                    ,VFun $ \(VLit x) -> VFun $ \(VLit y) -> VLit $ x*y
                    ,undefined
                ),
                (    "#div"
                    ,VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ DLit $ (fromIntegral x)/(fromIntegral y)
                    ,undefined
                ),
                (    "#gt"
                    ,VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> boolToVConstr (x>y)
                    ,TFun (TFun a a) (TConstr "Bool")
                ),
                (    "#eq"
                    ,VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> boolToVConstr (x==y)
                    ,TFun (TFun a a) (TConstr "Bool")
                ),
                (    "#not"
                    ,VFun $ \v -> boolToVConstr $ not $ vConstrToBool v
                    ,TFun (TConstr "Bool") (TConstr "Bool")
                ),
                (    "#or"
                    ,VFun $ \v1 -> VFun $ \v2 -> boolToVConstr $ (vConstrToBool v1) || (vConstrToBool v2)
                    ,TFun (TFun (TConstr "Bool") (TConstr "Bool")) 
                          (TConstr "Bool")
                ),
                (    "#bind"
                    ,VFun $ \(VIO a1) -> VFun $ \(VFun a2) -> VIO $ a1 >>= \s -> run $ a2 s  -- a1 >>= \s -> a2 s
                    ,undefined
                ),
                (    "#then"
                    ,VFun $ \(VIO a1) -> VFun $ \(VIO a2) -> VIO $ a1 >> a2
                    ,undefined
                ),
                (    "Undefined"
                    ,vConstructor "Undefined" 0 id
                    ,TApp (TConstr "Undefined") a
                ),
                (    "(,)"
                    ,vConstructor "(,)" 2 id
                    ,TApp (TApp (TConstr "(,)") a) a
                ),
                (    "(,,)"
                    ,vConstructor "(,,)" 3 id
                    ,TApp (TApp (TApp (TConstr "(,)") a) a) a
                ),
                (    "Cons"
                    ,vConstructor "Cons" 2 id
                    ,TFun a (TFun 
                                (TApp (TConstr "[]") a) 
                                (TApp (TConstr "[]") a))
                ),
                (    "Nil"
                    ,vConstructor "Nil" 0 id
                    ,TApp (TConstr "[]") a
                )
           ]
       where a = TVar "a"


vPrint :: [Char] -> IO Value
vPrint []     = return $ VConstr "()" []
vPrint (c:[]) = putChar c >> putChar '\n' >> vPrint []
vPrint (c:cs) = putChar c >> vPrint cs

vConstructor :: ConstrID -> Int -> ([Value] -> [Value]) -> Value
vConstructor cid n k
    | n < 0      = error "vConstructor must be called with n >= 0"
    | n == 0     = VConstr cid $ k []
    | otherwise  = VFun $ \v -> vConstructor cid (n-1) $ (. (v:)) k

run :: Value -> IO Value
run act = case act of
    VIO a -> a
    _     -> error "faulty type"

vConcat :: Value -> Value -> Value
vConcat (VConstr "Nil" []) v2 = v2
vConcat (VConstr "Cons" [v1, vs]) v2 = VConstr "Cons" [v1, (vConcat vs v2)]

boolToVConstr :: Bool -> Value
boolToVConstr b = VConstr (show b) []

vConstrToBool :: Value -> Bool
vConstrToBool (VConstr "True" []) = True
vConstrToBool (VConstr "False" []) = False