module TI where
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except
import Data.Map(Map)
import AST.DataTypes
import AST.Environment
import Control.Exception

class Types a where
    ftv    ::  a -> S.Set Var
    apply  ::  Subst -> a -> a


instance Types Type where
    ftv (TVar n)            = S.singleton n
    ftv (TFun t1 t2)        = ftv t1 `S.union` ftv t2
    ftv (TConstr _)         = S.empty
    ftv (TApp t1 t2)        = ftv t1 `S.union` ftv t2
    apply s (TVar n)        = fromMaybe (TVar n) (M.lookup n s)
    apply s (TFun t1 t2)    = TFun (apply s t1) (apply s t2)
    apply s (TApp t1 t2)    = TApp (apply s t1) (apply s t2)
    apply _ t               = t

instance Types Scheme where
    ftv (Scheme vars t)      =  ftv t `S.difference` S.fromList vars
    apply s (Scheme vars t)  =  Scheme vars (apply (foldr M.delete s vars) t)

instance Types a => Types [a] where
    apply s =  map (apply s)
    ftv     =  foldr (S.union . ftv) S.empty


-- A type t is actual wrt. a substitution s if no type variables in t are mapped in s
-- Invariant: All types in any substitution s must be actual wrt. s
type Subst = Map Var Type

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2   = M.map (apply s1) s2 `M.union` s1


--data TIEnv = TIEnv {} remove?
data TIState = TIState {tiSupply :: Int,
                        tiSubst  :: Subst}

type TI a = ExceptT String (State TIState) a

runTI :: TI a -> (Either String a, TIState)
runTI t = runState (runExceptT t) initTIState
    where initTIState = TIState{tiSupply = 0,
                                tiSubst = M.empty}


newTyVar :: String -> TI Type
newTyVar prefix = do
    s <- get
    put s{tiSupply = tiSupply s + 1}
    return (TVar  (prefix ++ show (tiSupply s)))

-- "Refresh" the type to make sure it is actual wrt. the current substitution
refresh :: Type -> TI Type
refresh t = do
  s <- gets tiSubst
  return (apply s t)

debugTI :: TI a
debugTI = do
    s <- gets tiSubst
    error $ show s

unify :: Type -> Type -> TI Type
unify t1 t2 = do
  go t1 t2
  where
    go (TFun l1 r1) (TFun l2 r2) = do
        m1 <- go l1 l2
        m2 <- go r1 r2
        return $ TFun m1 m2
    go (TVar u) t               = varBind u t
    go t (TVar u)               = varBind u t
    go (TConstr a) (TConstr b) | a == b = return $ TConstr a
    go (TApp l1 r1) (TApp l2 r2)= do
        m1 <- go l1 l2
        m2 <- go r1 r2
        return $ TApp m1 m2
    go (TConstr "Undefined") a  = return a
    go a (TConstr "Undefined")  = return a
    go e1 e2                    = throwError $ "types do not unify: " ++ show e1 ++
                                        " vs. " ++ show e2

unifyAll :: [Type] -> TI Type
unifyAll [t] = return t
unifyAll (t:ts) = unifyAll ts >>= unify t -- Does a lot of needless refreshing...


varBind :: Var -> Type -> TI Type
varBind u t = do
    u' <- refresh $ TVar u
    case u' of
        TVar u'' -> refresh t >>= varBind' u''
        _        -> unify u' t
    where
        varBind' :: Var -> Type -> TI Type
        varBind' u t | t == TVar u        =  return t
                     | u `S.member` ftv t =  throwError $ "occur check fails: " ++ u ++
                                                 " vs. " ++ show t
                     | otherwise          =  do
                            let newSubst = M.singleton u t
                            modify (\s -> s{tiSubst = composeSubst newSubst (tiSubst s) } )
                            return t

ti ::  TypeEnv -> Exp -> TI Type
ti (TypeEnv env) (EVar v) = case M.lookup v env of
        Nothing -> throwError $ "unbound variable: " ++ v
        Just v' -> instantiate v'
ti _ (ELit l)           = case l of
    ILit _ -> return $ TConstr "Int"
    DLit _ -> return $ TConstr "Double"
    CLit _ -> return $ TConstr "Char"
ti env (EUnOp o e) = case o of  -- TODO
    Not -> do
        t1 <- ti env e
        t2 <- ti env (EConstr "True")
        unify t1 t2
    _   -> throwError $ "Not a unary operator" -- Should this be here or in DataTypes?
ti env (EBinOp u e1 e2) = ti env (EApp (EApp (EVar (show u)) e1) e2)
ti env (ELam v e)         = do
    t0 <- newTyVar "a"
    let TypeEnv env' = remove v env
        env'' = TypeEnv (env' `M.union` M.singleton v (Scheme [] t0))
    t1 <- ti env'' e
    return (TFun t0 t1)
ti env (EConstr id) = lookupType env id
ti env (ECase e0 pes)     = do
    t0 <- ti env e0
    let go (p, e) = do
        let pvs = freeVarsP p
        env' <- declareAll pvs env
        tp   <- ti env' (patToExp p)
        unify tp t0
        ti env' e
    ts <- mapM go pes
    unifyAll ts
ti env (EApp e1 e2)       = do
    t1 <- ti env e1
    t2 <- ti env e2
    a  <- newTyVar "a"
    unify (TFun t2 a) t1
    return a
ti env (ELetIn v e1 e2)   = do
    t1 <- ti env e1
    let TypeEnv env' = remove v env
        t'           = generalize env t1
        env''        = TypeEnv (M.insert v t' env')
    ti env'' e2

progToTypeEnv :: Program -> TypeEnv
progToTypeEnv (Program dds fds) = TypeEnv $ M.fromList $ concatMap dDecl dds ++ map fDecl fds

fDecl :: FuncDecl -> (Var, Scheme)
fDecl (DFunc id t vs _) = (id, Scheme (S.toList (ftv t)) t)
--fDecl (DFunc id t vs _) = (id, Scheme (S.toList (ftv t)) t):(varsToScheme t vs)
--fDecl (DFunc id t vs _) = (id, (Scheme vs t))

{--varsToScheme :: Type -> [Var] -> [(Var, Scheme)]
varsToScheme _ []     = []
varsToScheme t (v:vs) = case t of
    TVar var     -> [(v, Scheme (S.toList (ftv t)) t)]
    TiVar var    -> [(v, Scheme (S.toList (ftv t)) t)]
    TConstr cid  -> [(cid, Scheme (S.toList (ftv t)) t)]
    TiConstr cid -> [(cid, Scheme (S.toList (ftv t)) t)]
    TFun t1 t2   -> (v, Scheme (S.toList (ftv t)) t1):(varsToScheme t2 vs)
    TApp t1 t2   -> (v, Scheme (S.toList (ftv t)) t1):(varsToScheme t2 vs)--}

-- (TFun (TVar "Int") (TFun (TVar "Int") (TFun (TVar "Int") (TVar "Int"))))

{--data Type =
        TVar Var
        | TiVar Var
        | TConstr ConstrID
        | TiConstr ConstrID
        | TFun Type Type
        | TApp Type Type--}

dDecl :: DataDecl -> [(ConstrID, Scheme)]
dDecl (DData id vars cs) = map (schemify . cDecl tres) cs
    where
        -- Check that there aren't free variables in vars
        schemify (id, t) = (id, Scheme vars t)
        tres             = foldl TApp (TConstr id) (map TVar vars)

cDecl :: Type -> ConstrDecl -> (ConstrID, Type)
cDecl t (ConstrDecl id ts) = (id, foldr TFun t ts)

infer :: TypeEnv -> Exp -> TI Type
infer env ex = ti env ex >>= refresh

    -- checks that no linear variables occurs more than once in an expression
linearCheck :: (M.Map Var Int) -> TypeEnv -> Exp -> Bool
linearCheck m te e =  let lst = (M.filterWithKey (\k v -> v /= 1 && (isLinear te k)) (lc e te m)) in
                        case M.size lst of
                            0       -> True
                            n       -> throw $ LinearException $ (show lst)

isLinear :: TypeEnv -> Var -> Bool
isLinear te v = case lookupType' te v of
    (Scheme vs t) -> isLinear' t
        where isLinear' t = case t of
                (TiVar _)         -> True
                (TiConstr _)      -> True
                (TVar ('i':_))    -> True
                (TConstr ('i':_)) -> True
                _                 -> False

lc ::  Exp -> TypeEnv -> (M.Map Var Int) -> (M.Map Var Int)
lc e tEnv m = case e of
    (EApp e1 e2)       -> lc e1 tEnv (lc e2 tEnv m)
    (EVar var)         -> if isLinear tEnv var
                          then M.insertWith (+) var 1 m
                          else m
    (ELit _)           -> m
    (EUnOp _ e)        -> lc e tEnv m
    (EBinOp _ e1 e2)   -> lc e1 tEnv (lc e2 tEnv m)
    (ELam var e)       -> if isLinear tEnv var
                          then M.insertWith (+) var 1 m
                          else m
    (EConstr cid)      -> if isLinear tEnv cid
                          then M.insertWith (+) cid 1 m
                          else m
    (ECase ex pes)     -> let env = lc ex tEnv m
                              es = (map snd pes)
                              lst = map (linearCheck env tEnv) es
                            in case (and lst) of
                                True  -> env
                                False -> error $ "m: " ++ (show m) ++ " es: " ++ (show es)  -- throw LinearException
    (ELetIn _ e1 e2)   -> lc e1 tEnv (lc e2 tEnv m)
    (EListComp e2 vvs e1) -> undefined


-- Returns the free expression variables in patterns
freeVarsP :: Pattern -> [Var]
freeVarsP (PConstr v vs) = concatMap freeVarsP vs
freeVarsP (PVar v)       = [v]
freeVarsP _              = []

-- Adds new type variables for each expression variables
declareAll :: [String] -> TypeEnv -> TI TypeEnv
declareAll []     env = return env
declareAll (x:xs) env = do
    a <- newTyVar "a"
    declareAll xs $ declareMono x a env

-- Converts a pattern to an expression
patToExp :: Pattern -> Exp
patToExp (PConstr v vs) = foldl EApp (EConstr v) (map patToExp vs)
patToExp (PVar v)       = EVar v
patToExp (PLit x)       = ELit x
patToExp PWild          = EVar "undefined"


newtype TypeEnv = TypeEnv (M.Map String Scheme)

remove :: Var -> TypeEnv -> TypeEnv
remove var (TypeEnv env) = TypeEnv (M.delete var env)

add :: Var -> Scheme -> TypeEnv -> TypeEnv
add v sch (TypeEnv m) = TypeEnv (M.insert v sch m)

instance Types TypeEnv where
    ftv (TypeEnv env)      =  ftv (M.elems env)
    apply s (TypeEnv env)  =  TypeEnv (M.map (apply s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = S.toList (ftv t `S.difference` ftv env)

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\ _ -> newTyVar "a") vars
    let s = M.fromList (zip vars nvars)
    refresh (apply s t)


-- Lookup a expression level variable in the environment
lookupType :: TypeEnv -> String -> TI Type
lookupType (TypeEnv m) v = case M.lookup v m of
  Nothing -> throwError $ "Undeclared variable " ++ v
  Just sch -> instantiate sch

lookupType' :: TypeEnv -> String -> Scheme
lookupType' (TypeEnv m) s = case (M.lookup s m) of
    Nothing   -> throw $ TypeException $ "Undeclared variable: " ++ s
    Just sch  -> sch

declarePoly :: String -> Type -> TypeEnv -> TypeEnv
declarePoly v t e = add  v (generalize e t) e

declareMono :: String -> Type -> TypeEnv -> TypeEnv
declareMono v t = add v (Scheme [] t)
