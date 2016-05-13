module AST.Interpreter where

import Data.Maybe
import Text.Parsec
import AST.Environment
import AST.DataTypes
import Data.Map
import qualified Data.Map as M

-- call interpret with io
interpret :: Program -> IO ()
interpret (Program dd fd) = do
    let v = interpret' dd fd
    case v of
        VIO v -> v >> return ()
        v     -> (putStrLn $ show v) >> return ()
    where
        interpret' dd fd = let e = addFuncDeclsToEnv e startEnv fd in
                        lookupInEnv (addDataDeclsToEnv e dd) "main"

addDataDeclsToEnv :: Env -> [DataDecl] -> Env
addDataDeclsToEnv env []                    = env
addDataDeclsToEnv env ((DData _ _ cds):dds) =
    addDataDeclsToEnv (addManyToEnv env (fst vvs) (snd vvs)) dds
        where vvs = unzip $ Prelude.map mkConstr cds


mkConstr :: ConstrDecl -> (String, Value)
mkConstr (ConstrDecl name ts) = (name, (vConstructor name (length ts) id))

-- addDecsToEnv is a helper function to interpret
-- Adds declarations to the environment
addFuncDeclsToEnv :: Env -> Env -> [FuncDecl] -> Env
addFuncDeclsToEnv env sEnv []     = sEnv
addFuncDeclsToEnv env sEnv (d:ds) = insertAll e' (makeBinding d env)
--addDecsToEnv env (d:ds) = uncurry M.insert (makeBinding d env) e'
    where
        e' = addFuncDeclsToEnv env sEnv ds

insertAll :: Env -> [(Var,Value)] -> Env
insertAll e []     = e
insertAll e ((var,val):vs) = insertAll (addToEnv e var val) vs

-- makeBinding is a helper function to addDecsToEnv
-- Makes bindings from declarations to environment
makeBinding :: FuncDecl -> Env -> [(Var, Value)]
makeBinding (DFunc name _ vs e) env = [(name, val)]
    where
        addLams [] e     = e
        addLams (v:vs) e = ELam v (addLams vs e)
        val              = eval env (addLams vs e)

-- startEnv creates the basic environment
startEnv :: Env
startEnv = startEnv' M.empty startEnvironment
    where
        startEnv' :: Env -> [(String, Value, Scheme)] -> Env
        startEnv' env [] = env
        startEnv' env ((a,b,c):xs) = startEnv' (M.insert a b env) xs

-- evaluation of an expression in an environment
eval :: Env -> Exp -> Value
eval env expr = case expr of
    ELetIn var e1 e2         -> eval env' e2
        where env' = addToEnv env var (eval env' e1)
    EConstr cid              -> lookupInEnv env cid
    EApp e1 e2               -> case (eval env e1) of
         VFun v1 -> v1 v2
            where v2 = eval env e2
         _       -> VConstr "Undefined" []
    ELam var e               -> VFun $ \v -> eval (addToEnv env var v) e
    EVar var                 -> lookupInEnv env var
    ELit lit                 -> VLit lit
    EUnOp op e               -> f $ eval env e
        where (VFun f) = lookupInEnv env (show op)
    EBinOp op e1 e2          -> f $ eval env e2
        where (VFun f') = lookupInEnv env (show op)
              (VFun f)  = f' $ eval env e1
    ECase expr' []           -> VLit (ILit 0)
    ECase expr' pEs          -> fromJust $ evalCase v env pEs
        where v = eval env expr'

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
match (PConstr pcid ps) (VConstr vcid vs)
    | pcid == vcid = matchConstr ps vs
    | otherwise    = Nothing

matchConstr :: [Pattern] -> [Value] -> Maybe [(Var,Value)]
matchConstr ps vs = fmap concat $ sequence (zipWith match ps vs)
