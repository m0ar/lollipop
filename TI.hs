
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import Data.Map(Map,insert)
import DataTypes
import qualified Text.PrettyPrint as PP




data Type =
    TVar Var
    | TInt
    | TDouble
    | TChar
    | TString
    | TFun Type Type
    deriving (Eq, Ord)

data Scheme = Scheme [Var] Type

class Types a where
    ftv    ::  a -> S.Set Var
    apply  ::  Subst -> a -> a


instance Types Type where
    ftv (TVar n)            = S.singleton n
    ftv TInt                = S.empty
    ftv TDouble             = S.empty
    ftv TChar               = S.empty
    ftv TString             = S.empty
    ftv (TFun t1 t2)        = ftv t1 `S.union` ftv t2
    apply s (TVar n)        = case M.lookup n s of
                               Nothing  -> TVar n
                               Just t   -> t
    apply s (TFun t1 t2)    = TFun (apply s t1) (apply s t2)
    apply s t               = t

instance Types Scheme where
    ftv (Scheme vars t)      =  (ftv t) `S.difference` (S.fromList vars)
    apply s (Scheme vars t)  =  Scheme vars (apply (foldr M.delete s vars) t)

instance Types a => Types [a] where
    apply s  =  map (apply s)
    ftv l    =  foldr S.union S.empty (map ftv l)


-- A type t is actual wrt. a substitution s if no type variables in t are mapped in s
-- Invariant: All types in any substitution s must be actual wrt. s
type Subst = Map Var Type

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2   = (M.map (apply s1) s2) `M.union` s1


data TIEnv = TIEnv {}
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


unify :: Type -> Type -> TI Type
unify t1 t2 = do
  t1' <- refresh t1 -- Maybe not all of these refreshes are needed (but better safe than sorry). 
  t2' <- refresh t2
  go t1' t2' >>= refresh
  where
    go (TFun l1 r1) (TFun l2 r2) = do  
        m1 <- go l1 l2
        m2 <- go r1 r2
        return $ TFun m1 m2
    go (TVar u) t              =  varBind u t
    go t (TVar u)              =  varBind u t
    go TInt TInt               =  return TInt
    go TDouble TDouble         =  return TDouble
    go TChar TChar             =  return TChar
    go TString TString         =  return TString
    go t1 t2                   =  throwError $ "types do not unify: " ++ show t1 ++ 
                                " vs. " ++ show t2

unifyAll :: [Type] -> TI Type
unifyAll [t] = return t
unifyAll (t:ts) = unifyAll ts >>= unify t -- Does a lot of needless refreshing...


varBind :: Var -> Type -> TI Type
varBind u t | t == TVar u        =  return t
            | u `S.member` ftv t =  throwError $ "occur check fails: " ++ u ++
                                         " vs. " ++ show t
            | otherwise          =  do 
                    let newSubst = M.singleton u t
                    modify (\s -> s{tiSubst = composeSubst newSubst (tiSubst s) } )
                    return t

ti ::  TypeEnv -> Exp -> TI Type
ti (TypeEnv env) (EVar v) = do
    case M.lookup v env of
        Nothing -> throwError $ "unbound variable: " ++ v
        Just v' -> instantiate v'
ti env (ELit l) = case l of
    ILit _ -> return TInt
    DLit _ -> return TDouble
    CLit _ -> return TChar
    SLit _ -> return TString
ti env (ELam v e) = undefined
ti env (ECase e0 pes) = do
    t0 <- ti env e0
    let go (p, e) = do
        let pvs = freeVarsP p
        env' <- declareAll pvs env
        tp   <- ti env' (patToExp p)
        unify tp t0
        ti env' e
    ts <- mapM go pes
    unifyAll ts
ti env (EApp e1 e2) = do
    t1 <- ti env e1
    t2 <- ti env e2
    a  <- newTyVar "a"
    unify (TFun t2 a) t1
    return a
ti env (ELetIn v e1 e2) = undefined

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


--infer e
--    t <- ti startEnv e
--    refresh t

patToExp :: Pattern -> Exp
patToExp (PConstr v vs) = foldl EApp (EConstr v) (map patToExp vs)
patToExp (PVar v)       = EVar v
patToExp (PLit x)       = ELit x






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
    where vars = S.toList ((ftv t) `S.difference` (ftv env))

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

declarePoly :: String -> Type -> TypeEnv -> TypeEnv
declarePoly v t e = add  v (generalize e t) e

declareMono :: String -> Type -> TypeEnv -> TypeEnv
declareMono v t e = add v (Scheme [] t) e 






instance Show Type where
    showsPrec _ x = shows (prType x)

prType :: Type -> PP.Doc
prType (TVar n)   = PP.text n
prType TInt       = PP.text "Int"
prType TDouble    = PP.text "Double"
prType TChar      = PP.text "Char"
prType TString    = PP.text "String"
prType (TFun t s) = prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType :: Type -> PP.Doc
prParenType t = case t of
                    TFun _ _  -> PP.parens (prType t)
                    _         -> prType t

