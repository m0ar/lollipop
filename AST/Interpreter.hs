module Interpreter where

import Data.Maybe
import Text.Parsec
import Environment
import DataTypes
import Data.Map
import qualified Data.Map as M

-- call interpret med io
interpret :: Program -> IO ()
interpret ds = do
            let v = interpret' ds
            case v of
                VIO v -> v >> return ()
                v     -> (putStrLn $ show v) >> return ()
    where
        interpret' ds = let e = addDecsToEnv e ds in
                        lookupInEnv e "main"

-- addDecsToEnv is a helper function to interpret

-- Adds declarations to the environment
addDecsToEnv :: Env -> [Declaration] -> Env
addDecsToEnv env []     = startEnv
addDecsToEnv env (d:ds) = uncurry M.insert (makeBinding d env) e'
    where
        e' = addDecsToEnv env ds

startEnv :: Env
startEnv = printF $ readLnF $ addF $ subF $ mulF $ bind $ M.empty
    where   printF  = M.insert "Print" $ VFun $ \(VString s) -> VIO $ print s >> return (VConstr "()" [])
            readLnF = M.insert "ReadLine" $ VIO $ fmap VString readLn
            subF    = M.insert "Sub" $ VFun $ \(VInt x) -> VFun $ \(VInt y) -> VInt $ x-y
            addF    = M.insert "Add" $ VFun $ \(VInt x) -> VFun $ \(VInt y) -> VInt $ x+y
            mulF    = M.insert "Mul" $ VFun $ \(VInt x) -> VFun $ \(VInt y) -> VInt $ x*y -- a1 >>= \s -> a2 s
            bind    = M.insert "Bind" $ VFun $ \(VIO a1) -> VFun $ \(VFun a2) -> VIO $ a1 >>= \s -> run $ a2 s

run :: Value -> IO Value
run act = case act of
    VIO a -> a >> return (VConstr "()" [])
    _     -> error "faulty type"


-- makeBinding is a helper function to addDecsToEnv
-- Makes bindings from declarations to environment
makeBinding :: Declaration -> Env -> (Var, Value)
makeBinding (DConstr id val) env  = (id, val)
makeBinding (DFunc name vs e) env = (name, eval env (addLams vs e))
        where
            addLams [] e     = e
            addLams (v:vs) e = ELam v (addLams vs e)


equals :: Value -> Value -> Bool
equals (VInt x) (VInt y) = x == y

-- evaluation of an expression in an environment
eval :: Env -> Exp -> Value
eval env expr = case expr of
        ELetIn var e1 e2         -> eval env' e2
            where env' = addToEnv env var (eval env' e1)
        EConstr cid              -> lookupInEnv env cid
        ECase e ps                -> eval env' e'
            where (VConstr cid vals)    = eval env e
                  (Constr cid' vars e') = findPattern ps cid
                  env'             = addManyToEnv env vars vals
        EApp e1 e2               -> case (eval env e1) of
             VFun v1                -> v1 v2
                where v2 = eval env e2
             _                      -> error "NOT FUNCTION!!!!"
        ELam var e               -> VFun f
            where f v = eval (addToEnv env var v) e
        EVar var                 -> (lookupInEnv env var)
        ELit (ILit i)            -> VInt i
        ELit (SLit s)            -> VString s
        EBinOp op e1 e2          -> f $ eval env e2
                where (VFun f') = lookupInEnv env (show op)
                      (VFun f) = f' $ eval env e1
        ETup2 e1 e2              -> VTup2 (eval env e1) (eval env e2)
        ETup3 e1 e2 e3           -> VTup3 (eval env e1) (eval env e2) (eval env e3)



-- finds pattern based on constructor ID
findPattern :: [Pattern] -> ConstrID -> Pattern
findPattern [] _                          = error "could not find pattern"
findPattern (p:[]) _                      = p
findPattern (p@(Constr cid vs expr):ps) cid'
                            | cid == cid' = p
                            | otherwise   = findPattern ps cid'
