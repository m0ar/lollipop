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
startEnv = printF $ readLnF $ addF $ mulF $ bindF $ powF
                  $ thenF $ true $ false $ tuple $ truple
                  $ nil $ cons $ undef $ gtF $ divF
                  $ eqF $ notF $ orF $ M.empty
    where   printF  = M.insert "print" $ VFun $ \(VString s) -> VIO $ print s >> return (VConstr "()" []) -- TODO remove VString
            readLnF = M.insert "readLine" $ VIO $ fmap VString readLn
            addF    = M.insert "#add" $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ ILit $ x+y
            powF    = M.insert "#pow" $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ DLit $ (fromIntegral x) ^^ y
            mulF    = M.insert "#mul" $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ ILit $ x*y
            divF    = M.insert "#div" $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> VLit $ DLit $ (fromIntegral x)/(fromIntegral y)
            gtF     = M.insert "#gt"  $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> boolToVConstr (x>y)
            eqF     = M.insert "#eq"  $ VFun $ \(VLit (ILit x)) -> VFun $ \(VLit (ILit y)) -> boolToVConstr (x==y)
            notF    = M.insert "#not" $ VFun $ \v -> boolToVConstr $ not $ vConstrToBool v
            orF     = M.insert "#or"  $ VFun $ \v1 -> VFun $ \v2 -> boolToVConstr $ (vConstrToBool v1) || (vConstrToBool v2)
            bindF   = M.insert "bind" $ VFun $ \(VIO a1) -> VFun $ \(VFun a2) -> VIO $ a1 >>= \s -> run $ a2 s  -- a1 >>= \s -> a2 s
            thenF   = M.insert "then" $ VFun $ \(VIO a1) -> VFun $ \(VIO a2) -> VIO $ a1 >> a2
            undef   = M.insert "Undefined" $ vConstructor "Undefined" 0 id
            true    = M.insert "True" $ vConstructor "True" 0 id
            false   = M.insert "False" $ vConstructor "False" 0 id
            tuple   = M.insert "(,)" $ vConstructor "(,)" 2 id
            truple  = M.insert "(,,)" $ vConstructor "(,,)" 3 id
            nil     = M.insert "Nil" $ vConstructor "Nil" 0 id
            cons    = M.insert "Cons" $ vConstructor "Cons" 2 id

vConstructor :: ConstrID -> Int -> ([Value] -> [Value]) -> Value
vConstructor cid n k
    | n < 0      = error "vConstructor must be called with n >= 0"
    | n == 0     = VConstr cid $ k []
    | otherwise  = VFun $ \v -> vConstructor cid (n-1) $ (. (v:)) k

run :: Value -> IO Value
run act = case act of
    VIO a -> a
    _     -> error "faulty type"

boolToVConstr :: Bool -> Value
boolToVConstr b = VConstr (show b) []

vConstrToBool :: Value -> Bool
vConstrToBool (VConstr "True" []) = True
vConstrToBool (VConstr "False" []) = False


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
        EUnOp op e               -> f $ eval env e
                where (VFun f) = lookupInEnv env (show op)
        EBinOp op e1 e2          -> f $ eval env e2
                where (VFun f') = lookupInEnv env (show op)
                      (VFun f) = f' $ eval env e1
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
match p v = matchConstr p v

-- help function for match which handledes pattern matching for constructors
matchConstr :: Pattern -> Value -> Maybe [(Var, Value)]
matchConstr (PConstr "Cons" [(PVar var), p@(PConstr pcid' ps')])
            (VConstr "Cons" [val, v@(VConstr vcid' vs')])
    | pcid' == vcid' = Just $ [(var,val)]++(matchCons p v)
    | otherwise      = Nothing
matchConstr (PConstr pcid ps) (VConstr vcid vs)
    | pcid == vcid = fmap concat $ sequence (zipWith match ps vs)
    | otherwise    = Nothing

-- helper function for matchConstr which binds at the end of a list
matchCons :: Pattern -> Value -> [(Var, Value)]
matchCons (PConstr "Nil" _) _                  = []
matchCons (PConstr "Cons" ((PVar var):vs)) val = [(var, val)]
