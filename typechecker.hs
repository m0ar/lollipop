-- Based on github.com/mgrabmueller/AlgorithmW
-- expanded to fit lollipop core syntax

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import DataTypes
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.PrettyPrint as PP

data Type = TVar String
    | TInt
    | TFun Type Type
    deriving (Eq, Ord)

data Scheme = Scheme [String ] Type

type Subst = M.Map String Type

class Types a where
    ftv    ::  a -> S.Set String
    apply  ::  Subst -> a -> a


instance Types Type where
    ftv (TVar n)      =  S.singleton n
    ftv TInt          =  S.empty
--    ftv TBool         =  S.empty
    ftv (TFun t1 t2)  =  ftv t1 `S.union` ftv t2
    apply s (TVar n)      =  case M.lookup n s of
                               Nothing  -> TVar n
                               Just t   -> t
    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
    apply s t             =  t


instance Types Scheme where
    ftv (Scheme vars t)      =  (ftv t) `S.difference` (S.fromList vars)
    apply s (Scheme vars t)  =  Scheme vars (apply (foldr M.delete s vars) t)

instance Types a => Types [a] where
    apply s  =  map (apply s)
    ftv l    =  foldr S.union S.empty (map ftv l)

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2   = (M.map (apply s1) s2) `M.union` s1


newtype TypeEnv = TypeEnv (M.Map String Scheme)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

instance Types TypeEnv where
    ftv (TypeEnv env)      =  ftv (M.elems env)
    apply s (TypeEnv env)  =  TypeEnv (M.map (apply s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = S.toList ((ftv t) `S.difference` (ftv env))

data TIEnv = TIEnv {}
data TIState = TIState {tiSupply :: Int,
                        tiSubst  :: Subst}

type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = do 
        (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
        return (res, st)
    where initTIEnv   = TIEnv{}
          initTIState = TIState{tiSupply = 0,
                                tiSubst = M.empty}


newTyVar :: String -> TI Type
newTyVar prefix = do
    s <- get
    put s{tiSupply = tiSupply s + 1}
    return (TVar  (prefix ++ show (tiSupply s)))


instantiate :: Scheme -> TI Type 
instantiate (Scheme vars t) = do  
    nvars <- mapM (\ _ -> newTyVar "a") vars
    let s = M.fromList (zip vars nvars)
    return $ apply s t


mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r') = do  
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s1 `composeSubst` s2)
mgu (TVar u) t              =  varBind u t
mgu t (TVar u)              =  varBind u t
mgu TInt TInt               =  return nullSubst
--mgu TBool TBool             =  return nullSubst
mgu t1 t2                   =  throwError $ "types do not unify: " ++ show t1 ++ 
                                " vs. " ++ show t2


varBind :: String -> Type -> TI Subst
varBind u t | t == TVar u        =  return nullSubst
            | u `S.member` ftv t =  throwError $ "occur check fails: " ++ u ++
                                         " vs. " ++ show t
            | otherwise          =  return (M.singleton u t)


tiLit :: TypeEnv -> Lit -> TI (Subst, Type)
tiLit _ (ILit _)  = return (nullSubst, TInt)
--tiLit _ (LBool _) = return (nullSubst, TBool)


ti ::  TypeEnv -> Exp -> TI (Subst, Type)
ti (TypeEnv env) (EVar n) = 
    case M.lookup n env of
       Nothing    -> throwError $ "unbound variable: " ++ n
       Just sigma -> do t <- instantiate sigma
                        return (nullSubst, t)
ti env (ELit l) = tiLit env l
ti env (ELam n e) = do 
    tv <- newTyVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv (env' `M.union` (M.singleton n (Scheme [] tv)))
    (s1, t1) <- ti env'' e
    return (s1, TFun (apply s1 tv) t1)
ti env (EApp e1 e2) = do 
    tv <- newTyVar "a"
    (s1, t1) <- ti env e1
    (s2, t2) <- ti (apply s1 env) e2
    s3 <- mgu (apply s2 t1) (TFun t2 tv)
    return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
ti env (ELetIn x e1 e2) = do
    (s1, t1) <- ti env e1
    let TypeEnv env' = remove env x
        t'           = generalize (apply s1 env) t1
        env''        = TypeEnv (M.insert x t' env')
    (s2, t2) <- ti (apply s1 env'') e2
    return (s1 `composeSubst` s2, t2)
{-ti env (ECase e1 es) = do
    (s1, t1) <- ti env e1
    -- ti for patterns
    ls       <- mapM (ti env . snd) es -- :: [(sub, type)] -}

patToExp :: Pattern -> Exp
patToExp (PConstr v vs) = foldl EApp (EConstr v) xs
patToExp (PVar v)       = EVar v
patToExp (PLit x)       = ELit x



typeInference :: M.Map String Scheme -> Exp -> TI Type
typeInference env e = do
    (s, t) <- ti (TypeEnv env) e
    return (apply s t)


e0  =  ELetIn "id" (ELam "x" (EVar "x"))
        (EVar "id")
e1  =  ELetIn "id" (ELam "x" (EVar "x"))
        (EApp (EVar "id") (EVar "id"))
e2  =  ELetIn "id" (ELam "x" (ELetIn "y" (EVar "x") (EVar "y")))
        (EApp (EVar "id") (EVar "id"))
e3  =  ELetIn "id" (ELam "x" (ELetIn "y" (EVar "x") (EVar "y")))
        (EApp (EApp (EVar "id") (EVar "id")) (ELit (ILit 2)))
e4  =  ELetIn "id" (ELam "x" (EApp (EVar "x") (EVar "x")))
        (EVar "id")
--e5  =  ELam "m" (ELetIn "y" (EVar "m")
--                 (ELetIn "x" (EApp (EVar "y") (ELit (LBool True)))
--                       (EVar "x")))


main :: IO ()
main = mapM_ test' [e0, e1, e2, e3, e4]


instance Show Type where
    showsPrec _ x = shows (prType x)


prType :: Type -> PP.Doc
prType (TVar n)   = PP.text n
prType TInt       = PP.text "Int"
--prType TBool      = PP.text "Bool"
prType (TFun t s) = prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType :: Type -> PP.Doc
prParenType t = case t of
                    TFun _ _  -> PP.parens (prType t)
                    _         -> prType t

--instance Show Exp where
--    showsPrec _ x = shows (prExp x)

prExp :: Exp -> PP.Doc
prExp (EVar name)     = PP.text name
prExp (ELit lit)      = prLit lit
prExp (ELetIn x b body) = PP.text "let" PP.<+> 
                        PP.text x PP.<+> PP.text "=" PP.<+>
                        prExp b PP.<+> PP.text "in" PP.$$
                        PP.nest 2 (prExp body)
prExp (EApp e1 e2)    = prExp e1 PP.<+> prParenExp e2
prExp (ELam n e)      = PP.char '\\' PP.<+> PP.text n PP.<+>
                        PP.text "->" PP.<+> prExp e
                                                                   
prParenExp :: Exp -> PP.Doc
prParenExp t = case t of
                    ELetIn _ _ _  -> PP.parens (prExp t)
                    EApp _ _    -> PP.parens (prExp t)
                    ELam _ _    -> PP.parens (prExp t)
                    _           -> prExp t

--instance Show Lit where
--    showsPrec _ x = shows (prLit x)

prLit :: Lit -> PP.Doc
prLit (ILit i)  = PP.int i
--prLit (LBool b) = if b then PP.text "True" else PP.text "False"

instance Show Scheme where
    showsPrec _ x = shows (prScheme x)

prScheme :: Scheme -> PP.Doc
prScheme (Scheme vars t) = PP.text "All" PP.<+>
                           PP.hcat (PP.punctuate PP.comma (map PP.text vars))
                           PP.<> PP.text "." PP.<+> prType t


test' :: Exp -> IO ()
test' e = do 
    (res, _) <- runTI (bu S.empty e)
    case res of
        Left err -> putStrLn $ "error: " ++ err
        Right t  -> putStrLn $ show e ++ " :: " ++ show t

data Constraint = CEquivalent Type Type
                | CExplicitInstance Type Scheme
                | CImplicitInstance Type (S.Set String) Type


instance Show Constraint where
    showsPrec _ x = shows (prConstraint x)


prConstraint :: Constraint -> PP.Doc
prConstraint (CEquivalent t1 t2) = 
    PP.hsep [prType t1, PP.text "=", prType t2]
prConstraint (CExplicitInstance t s) =
    PP.hsep [prType t, PP.text "<~", prScheme s]
prConstraint (CImplicitInstance t1 m t2) =
    PP.hsep [prType t1, 
             PP.text "<=" PP.<> 
                PP.parens (PP.hcat (PP.punctuate PP.comma (map PP.text (S.toList m)))), 
             prType t2]

type Assum = [(String, Type)]
type CSet = [Constraint]


bu :: S.Set String -> Exp -> TI (Assum, CSet, Type)
bu m (EVar n) = do 
    b <- newTyVar "b"
    return ([(n, b)], [], b)
bu m (ELit (ILit _)) = do 
    b <- newTyVar "b"
    return ([], [CEquivalent b TInt], b)
--bu m (ELit (LBool _)) = do 
--    b <- newTyVar "b"
--    return ([], [CEquivalent b TBool], b)
bu m (EApp e1 e2) = do 
    (a1, c1, t1) <- bu m e1
    (a2, c2, t2) <- bu m e2
    b <- newTyVar "b"
    return (a1 ++ a2, c1 ++ c2 ++ [CEquivalent t1 (TFun t2 b)], b)
bu m (ELam x body) = do 
    b@(TVar vn) <- newTyVar "b"
    (a, c, t) <- bu (vn `S.insert` m) body
    return (a `removeAssum` x, c ++ [CEquivalent t' b | (x', t') <- a,
                                     x == x'], TFun b t)
bu m (ELetIn x e1 e2) = do 
    (a1, c1, t1) <- bu m e1
    (a2, c2, t2) <- bu (x `S.delete` m) e2
    return (a1 ++ removeAssum a2 x,
            c1 ++ c2 ++ [CImplicitInstance t' m t1 |
                            (x', t') <- a2, x' == x], t2)


removeAssum [] _ = []
removeAssum ((n', _) : as) n | n == n' = removeAssum as n
removeAssum (a:as) n = a : removeAssum as n