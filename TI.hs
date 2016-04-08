
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except
import Data.Map(Map)
import AST.DataTypes as DataTypes
import qualified Text.PrettyPrint as PP

data Type =
    TVar Var
    | TInt
    | TDouble
    | TChar
    | TString
    | TBool
    | TList Type
    | TFun Type Type
    | TConstr String
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
    ftv TBool               = S.empty
    ftv (TFun t1 t2)        = ftv t1 `S.union` ftv t2
    ftv (TConstr _)         = S.empty
    ftv (TList t)           = ftv t
    apply s (TVar n)        = fromMaybe (TVar n) (M.lookup n s)
    apply s (TFun t1 t2)    = TFun (apply s t1) (apply s t2)
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
    go (TVar u) t               = varBind u t
    go t (TVar u)               = varBind u t
    go TInt TInt                = return TInt
    go TDouble TDouble          = return TDouble
    go TChar TChar              = return TChar
    go TString TString          = return TString
    go TBool TBool              = return TBool
    go (TList a) (TList b)      = unify a b
    go e1 e2                    = throwError $ "types do not unify: " ++ show e1 ++
                                        " vs. " ++ show e2

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
ti (TypeEnv env) (EVar v) = case M.lookup v env of
        Nothing -> throwError $ "unbound variable: " ++ v
        Just v' -> instantiate v'
ti _ (ELit l)           = case l of
    ILit _ -> return TInt
    DLit _ -> return TDouble
    CLit _ -> return TChar
    SLit _ -> return TString
ti env (EUnOp o e) = case o of
    Not -> do 
        t1 <- ti env e 
        t2 <- ti env (EConstr "True")
        unify t1 t2
ti env (EBinOp u e1 e2) = undefined -- TODO
ti env (ELam v e)         = do
    t0 <- newTyVar "a"
    let TypeEnv env' = remove v env
        env'' = TypeEnv (env' `M.union` M.singleton v (Scheme [] t0))
    t1 <- ti env'' e
    return (TFun t0 t1)
ti env (EConstr id) = case id of
    "False" -> return TBool
    "True"  -> return TBool
    _ -> return (TConstr id)
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
--    case e1 of
--        EConstr "Cons" -> return t2
--        _              -> case e2 of
--            EConstr "Nil" -> return t1
--            _             -> unify (TFun t2 a) t1
    unify (TFun t2 a) t1
    return a
ti env (ELetIn v e1 e2)   = do
    t1 <- ti env e1
    let TypeEnv env' = remove v env
        t'           = generalize env t1
        env''        = TypeEnv (M.insert v t' env')
    ti env'' e2

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

--infer e
--    t <- ti startEnv e
--    refresh t



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

declarePoly :: String -> Type -> TypeEnv -> TypeEnv
declarePoly v t e = add  v (generalize e t) e

declareMono :: String -> Type -> TypeEnv -> TypeEnv
declareMono v t = add v (Scheme [] t)




instance Show Type where
    showsPrec _ x = shows (prType x)

prType :: Type -> PP.Doc
prType (TVar n)   = PP.text n
prType TInt       = PP.text "Int"
prType TDouble    = PP.text "Double"
prType TChar      = PP.text "Char"
prType TString    = PP.text "String"
prType TBool      = PP.text "Bool"
prType (TConstr s)= PP.text ("Constructor " ++ s)
prType (TFun t s) = prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType :: Type -> PP.Doc
prParenType t = case t of
                    TFun _ _  -> PP.parens (prType t)
                    _         -> prType t



 -- TESTING --


printTestExp :: Exp -> IO()
printTestExp e = putStrLn $ testExp e

testExp :: Exp -> String
testExp e = case runTI (ti (TypeEnv M.empty) e) of
        (Left error,_) -> show e ++ "\n-- ERROR: " ++ error
        (Right t,_)    -> show e ++ " :: " ++ show t

eZero   = ELit (ILit 0)
eOne    = ELit (ILit 1)
eTwo    = ELit (ILit 2)
eThree  = ELit (ILit 3)
eFour   = ELit (ILit 4)
eFive   = ELit (ILit 5)
eSix    = ELit (ILit 6)
eSeven  = ELit (ILit 7)
eEight  = ELit (ILit 8)
eNine   = ELit (ILit 9)
x       = EVar "x"
y       = EVar "x"
z       = EVar "x"

-- Cons 5 Nil -> [5]
list1 = EApp (EApp (EConstr "Cons") eFive) (EConstr "Nil")

-- Cons 5 (Cons 2 Nil) -> [5,2]
list2 = EApp
            (EApp (EConstr "Cons") eFive)
            (EApp (EApp (EConstr "Cons") eTwo) (EConstr "Nil"))

-- Cons 5 (Cons 2 (Cons 3 Nil)) -> [5,2,3]
list3 = EApp
            (EApp (EConstr "Cons") eFive)
            (EApp (EApp (EConstr "Cons") eTwo)
            (EApp (EApp (EConstr "Cons") eThree) (EConstr "Nil")))

-- Cons 5 (Cons 2 (Cons 3 (Cons 1))) -> [5,2,3,1]
list4 = EApp
            (EApp (EConstr "Cons") eFive)
            (EApp (EApp (EConstr "Cons") eTwo)
            (EApp (EApp (EConstr "Cons") eThree)
            (EApp (EApp (EConstr "Cons") eOne) (EConstr "Nil"))))

-- test expressions
te1  = ELit $ ILit 3


te2  = ELetIn "id" (ELam "x" (EVar "x"))
       (EVar "id")

te3  = ELetIn "id" (ELam "x" (EVar "x"))
       (EApp (EVar "id") (EVar "id"))

te4  = ELetIn "id" (ELam "x" (ELetIn "y" (EVar "x") (EVar "y")))
       (EApp (EVar "id") (EVar "id"))

te5  = ELetIn "id" (ELam "x" (ELetIn "y" (EVar "x") (EVar "y")))
       (EApp (EApp (EVar "id") (EVar "id")) (ELit (ILit 2)))

te6  = ELetIn "id" (ELam "x" (EApp (EVar "x") (EVar "x")))
       (EVar "id")

-- should Succeed
te7  = ECase (ELit(ILit 2)) [(PLit(ILit 2), ELit(CLit 'i')) ,
                             (PLit(ILit 3), ELit(CLit 'u'))
                            ]

-- should Fail, different output types
te8  = ECase (ELit(ILit 2)) [(PLit(ILit 2), ELit(CLit 'i')) ,
                             (PLit(ILit 6), ELit(ILit 9))
                            ]

-- should Fail, different input and matching types
te9  = ECase (ELit(CLit '2')) [(PLit(ILit 2), ELit(CLit 'i')) ,
                               (PLit(ILit 3), ELit(CLit 'u'))
                              ]

te10 = EConstr "True"

te11 = EUnOp Not $ EConstr "False"



--te11 = EApp (EVar "sum") list1
--           where p1    = PConstr "Cons" ["x", "xs2"]
--                 e1    = EBinOp Add (EVar "x")
--                         (EApp (EVar "sum") (EVar "xs2"))
--                 p2    = PConstr "Nil" []
--                 ecase = ECase (EVar "xs") [(p1, e1), (p2, eZero)]
--                 dSum  = DFunc "sum" ["xs"] ecase

main = putStrLn $ 
    "\n --- TESTING EXPRESSIONS --- \n\n" ++
    concatMap ((++ "\n\n") . testExp)
        [te1,te2,te3,te4,te5,te6,te7,te8,te9]
