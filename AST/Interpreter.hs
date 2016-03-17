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
    where   printF  = M.insert "Print" $ VFun $ \(VString s) -> VIO $ print s >> return (VConstr "()" []) -- TODO remove VString
            readLnF = M.insert "ReadLine" $ VIO $ fmap VString readLn
            subF    = M.insert "Sub" $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ ILit $ x-y
            addF    = M.insert "Add" $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ ILit $ x+y
            mulF    = M.insert "Mul" $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ ILit $ x*y -- a1 >>= \s -> a2 s
            bind    = M.insert "Bind" $ VFun $ \(VIO a1) -> VFun $ \(VFun a2) -> VIO $ a1 >>= \s -> run $ a2 s

run :: Value -> IO Value
run act = case act of
    VIO a -> a
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
equals (VLit (ILit x)) (VLit (ILit y)) = x == y
equals (VLit (DLit x)) (VLit (DLit y)) = x == y
equals (VLit (SLit x)) (VLit (SLit y)) = x == y
equals (VLit (CLit x)) (VLit (CLit y)) = x == y
equals _                _              = False

-- evaluation of an expression in an environment
eval :: Env -> Exp -> Value
eval env expr = case expr of
        ELetIn var e1 e2         -> eval env' e2
            where env' = addToEnv env var (eval env' e1)
        EConstr cid              -> lookupInEnv env cid
        {-- ECase e ps                -> eval env' e'
            where (VConstr cid vals)    = eval env e
                  (Constr cid' vars e') = findPattern ps cid
                  env'             = addManyToEnv env vars vals --}
        EApp e1 e2               -> case (eval env e1) of
             VFun v1                -> v1 v2
                where v2 = eval env e2
             _                      -> error "NOT FUNCTION!!!!"
        ELam var e               -> VFun f
            where f v = eval (addToEnv env var v) e
        EVar var                 -> (lookupInEnv env var)
        ELit (SLit s)            -> VString s -- TODO remove
        ELit lit                 -> VLit lit
        EBinOp op e1 e2          -> f $ eval env e2
                where (VFun f') = lookupInEnv env (show op)
                      (VFun f) = f' $ eval env e1
        ETup2 e1 e2              -> VTup2 (eval env e1) (eval env e2)
        ETup3 e1 e2 e3           -> VTup3 (eval env e1) (eval env e2) (eval env e3)
        ECase expr' pEs          -> fromJust $ evalCase expr' env pEs

evalCase :: Exp -> Env -> [(Pattern, Exp)] -> Maybe Value
evalCase _ _ []            = Nothing
evalCase expr' e ((p, expr):pes) = case p of
    --PLit lit      -> if equalsLitVal lit (lookupInEnv e cid)
    PLit lit      -> if lit == lit'
                        then Just $ eval e expr
                        else evalCase expr' e pes
        where (VLit lit') = eval e expr'
    PConstr cid' vars -> if cid' == cid
                        then Just $ eval e' expr
                        else evalCase expr' e pes
        where e' = addManyToEnv e vars vals
              (VConstr cid vals) = eval e expr'
    PVar var     -> Just $ eval e' expr
        where e' = addToEnv e var v
    PWild             -> Just $ eval e expr
  where v = eval e expr'
        --(VConstr cid vals) = eval e expr'

-- checks if the value of a lit is the same as the value of the Value
{-- equalsLitVal :: Lit -> Value -> Bool
equalsLitVal (SLit x) (VString x') = x == x'
equalsLitVal (ILit x) (VInt x')    = x == x'
equalsLitVal (DLit x) (VDouble x') = x == x'
equalsLitVal (CLit x) (VChar x')   = x == x'
equalsLitVal _ _                   = error "incompatible types"
--}
-- finds pattern based on constructor ID
{-- findPattern :: [Pattern] -> ConstrID -> Pattern
findPattern [] _                          = error "could not find pattern"
findPattern (p:[]) _                      = p
findPattern (p@(Constr cid vs):ps) cid'
                            | cid == cid' = p
                            | otherwise   = findPattern ps cid' --}
