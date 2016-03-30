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
--  D.EApp ((D.EApp (D.EConstr "Cons") (D.ELit $ (D.ILit 1)))) (D.EApp ((D.EApp (D.EConstr "Cons") (D.ELit $ (D.ILit 2)))) (VConstr "Nil" []))
--  --> VConstr "Cons" [(VConstr "Cons" [(VConstr "Nil" []),(VLit 2)]), (VLit 1)]

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

-- (x:[]) -> D.PConstr "Cons" [(D.PVar "x"),(D.PConstr "Nil" [])]
-- [1,2] -> D.EApp ((D.EApp (D.EConstr "Cons") (D.ELit $ (D.ILit 1)))) (D.EApp ((D.EApp (D.EConstr "Cons") (D.ELit $ (D.ILit 2)))) (D.EConstr "Nil"))
--       -> VConstr "Cons" [(VConstr "Cons" [(VConstr "Nil" []),(VLit 2)]), (VLit 1)] ??

-- evalCase is a helper function to eval.
evalCase :: Value -> Env -> [(Pattern, Exp)] -> Maybe Value
evalCase _ _ []              = Nothing
evalCase v env ((p, expr):pes) = case match p v of
    Just vvs    -> Just $ eval env' expr
        where
            vvs' = unzip vvs    -- ([vars], [vals])
            env' = addManyToEnv env (fst vvs') (snd vvs')
    Nothing     -> evalCase v env pes

-- match is a helper function to evalCase. It takes a pattern and a value and
-- returns the bidings introduced by the patterns (Nothing if the value doesn't
-- match the pattern).
match :: Pattern -> Value -> Maybe [(Var, Value)]
match PWild _ = Just []
match (PLit pl) (VLit vl)
    | pl == vl  = Just []
    | otherwise = Nothing
match (PVar pv) var = Just [(pv, var)]
match p@(PConstr pcid ps) v@(VConstr vcid vs) = matchConstr p v

matchConstr :: Pattern -> Value -> Maybe [(Var, Value)]
matchConstr (PConstr "Cons" ps) (VConstr "Cons" vs)
    | pcid' == vcid' = case mc of
            Nothing -> Just [(var, val)]
            Just vv -> Just [(var, val), vv]  --matchConstr ps vs
    | otherwise      = Nothing
  where
    (PVar var)            = (!!) ps 0
    p@(PConstr pcid' ps') = (!!) ps 1
    val                   = (!!) vs 0
    v@(VConstr vcid' vs') = (!!) vs 1
    mc = matchCons p v
matchConstr (PConstr pcid ps) (VConstr vcid vs)
    | pcid == vcid = fmap concat $ sequence (zipWith match ps vs)
    | otherwise    = Nothing

matchCons :: Pattern -> Value -> Maybe (Var, Value)
matchCons (PConstr "Nil" _) val   = Nothing
matchCons (PConstr "Cons" ps) val = Just (var, val)
    where
      (PVar var)            = (!!) ps 0
