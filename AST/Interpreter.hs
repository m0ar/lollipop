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
startEnv = printF $ readLnF $ addF $ subF $ mulF $ bind $ true $ false $ tuple $ M.empty
    where   printF  = M.insert "print" $ VFun $ \(VString s) -> VIO $ print s >> return (VConstr "()" []) -- TODO remove VString
            readLnF = M.insert "readLine" $ VIO $ fmap VString readLn
            subF    = M.insert "#sub" $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ ILit $ x-y
            addF    = M.insert "#add" $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ ILit $ x+y
            mulF    = M.insert "#mul" $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ ILit $ x*y -- a1 >>= \s -> a2 s
            bind    = M.insert "#bind" $ VFun $ \(VIO a1) -> VFun $ \(VFun a2) -> VIO $ a1 >>= \s -> run $ a2 s
            true    = M.insert "True" $ vConstructor "True" 0 []
            false   = M.insert "False" $ vConstructor "False" 0 []
            tuple   = M.insert "(,)" $ vConstructor "(,)" 2 []
            --tuple   = M.insert "(,)" $ VFun $ (\v1 -> VFun $ \v2 -> VConstr "(,)" [v1,v2])

vConstructor :: ConstrID -> Int -> [Value] -> Value
vConstructor cid 0 vs = VConstr cid vs
vConstructor cid n vs = VFun (\v -> (vConstructor cid (n-1) (vs++[v])))


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
        EConstr cid              -> (lookupInEnv env cid)
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
        ECase expr' []           -> VLit (ILit 0)
        ECase expr' pEs          -> fromJust $ evalCase v env pEs
            where v = eval env expr'

evalCase :: Value -> Env -> [(Pattern, Exp)] -> Maybe Value
evalCase _ _ []              = Nothing
evalCase v e ((p, expr):pes) = case p of
    PLit lit          -> if lit == lit'
                         then Just $ eval e expr
                         else evalCase v e pes
        where (VLit lit') = v
    PConstr cid' vars -> if cid' == cid
                         then Just $ eval e' expr
                         else evalCase v e pes
        where e' = addManyToEnv e vars vals
              (VConstr cid vals) = v
    PVar var          -> Just $ eval e' expr
        where e' = addToEnv e var v
    PWild             -> Just $ eval e expr

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
