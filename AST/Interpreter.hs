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
startEnv = printF $ readLnF $ addF $ subF $ mulF $ bind $ true $ false $ tuple $ truple $ nil $ cons $ M.empty
    where   printF  = M.insert "print" $ VFun $ \(VString s) -> VIO $ print s >> return (VConstr "()" []) -- TODO remove VString
            readLnF = M.insert "readLine" $ VIO $ fmap VString readLn
            subF    = M.insert "#sub" $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ ILit $ x-y
            addF    = M.insert "#add" $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ ILit $ x+y
            mulF    = M.insert "#mul" $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ ILit $ x*y -- a1 >>= \s -> a2 s
            bind    = M.insert "#bind" $ VFun $ \(VIO a1) -> VFun $ \(VFun a2) -> VIO $ a1 >>= \s -> run $ a2 s
            true    = M.insert "True" $ vConstructor "True" 0 []
            false   = M.insert "False" $ vConstructor "False" 0 []
            tuple   = M.insert "(,)" $ vConstructor "(,)" 2 []
            truple  = M.insert "(,,)" $ vConstructor "(,,)" 3 []
            nil     = M.insert "Nil" $ vConstructor "Nil" 0 []
            cons    = M.insert "Cons" $ vConstructor "Cons" 2 []

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
    PConstr cid' ps -> if cid' == cid
                         then (case cid' of
                             "Cons" -> case (consCase p vals expr e) of
                                 Nothing -> evalCase v e pes
                                 v       -> v
                             "(,)"  -> tplCase vals e' expr
                             "(,,)" -> tplCase vals e' expr
                             _      -> Just $ eval e' expr )
                         else evalCase v e pes
        where e' = addManyToEnv e vars vals
              vars = Prelude.map patternToVar ps
              (VConstr cid vals) = v
              patternToVar p = case p of
                  (PVar v)      -> v
                  _             -> ""
              tplCase vals e' expr = case (evalTpls (zip ps vals) e' expr) of
                                          Nothing -> evalCase v e pes
                                          v       -> v

    PVar var          -> Just $ eval e' expr
        where e' = addToEnv e var v
    PWild             -> Just $ eval e expr

{--LPattern2 (PId (Id "x")) LPattern3)
PConstr "Cons" [(PVar "x"), (PConstr "Nil" [])]
(x:[])

PConstr "Cons" [(PLit 1), (PConstr "Cons" [(PLit 2),(PConstr "Nil" [])])]--}


consCase (PConstr "Nil" _) vs expr env = case vs of
    []                   -> Just $ eval env expr
    [(VConstr "Nil" [])] -> Just $ eval env expr
    _                    -> Nothing --error ("Nil not end of list. \n vs:  " ++ (concatMap show vs))
consCase (PConstr "Cons" [p,p']) (v:vs) expr env = case p of
                --PConstr pid ps ->
                PWild    -> consCase p' vs expr env
                PVar var -> consCase p' vs expr (addToEnv env var v)
                PLit lit -> if lit == ((\(VLit l) -> l) v)
                            then consCase p' vs expr env
                            else Nothing

evalTpls psVs e' expr = if and $ Prelude.map evalTpl psVs
                        then Just $ eval (bindEnv psVs e') expr
                        else Nothing
        where evalTpl ((PWild),_)              = True
              evalTpl ((PVar var),val)         = True
              evalTpl ((PLit lit),(VLit lit')) = lit == lit'
              bindEnv [] e'       = e'
              bindEnv (p:psVs) e' = case p of
                  ((PVar var),val) -> bindEnv psVs (addToEnv e' var val)
                  _                -> bindEnv psVs e'
