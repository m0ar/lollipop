-- To play around a bit with your interpreter
-- I wrote this little front end for the various
-- parts of your program.
-- to run : runghc -iAST/:grammar/ loli.hs
{-# LANGUAGE DeriveDataTypeable #-}
module Loli where

import System.IO
import AST.Interpreter
import Converter hiding (main)
import AST.DataTypes
import AST.Environment
import qualified AbsGrammar as A
import TI
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

import LexGrammar
import ParGrammar
import LayoutGrammar
import qualified AbsGrammar as G
import ErrM
import Control.Exception
import Data.Typeable
import Data.Either

import Data.Map
import qualified Data.Map as M

import System.Environment as E

myLLexer = resolveLayout True . myLexer

main = do
    E.getArgs >>= \s -> case s of
        [file] -> buildEnv file >>= uncurry (repl file)
        []     -> repl "" startEnv (tiStartEnv (TypeEnv M.empty) startEnvironment)
        _      -> putStrLn "Invalid arguments"

repl :: String -> Env -> TypeEnv -> IO ()
repl file env tEnv = do
    let loop = repl file env tEnv
    putStr (file ++ ">") >> hFlush stdout
    i <- getLine
    flip catch (\e -> print (e :: LoliException) >> loop) $ case i of
        "" -> loop
        ":q" -> return ()
        ":r" -> buildEnv file >>= uncurry (repl file)
        (':':'t':' ':s) -> putStrLn (show (lookupType' tEnv s))
                               >> (repl file env tEnv)
        (':':'l':s) -> case words s of
            [newfile] -> do
            res <- try $ buildEnv newfile
            case (res :: Either LoliException (Env, TypeEnv)) of
                Right (env, tEnv) -> repl newfile env tEnv
                Left  err         -> repl "" env tEnv
        _ -> case pExp (myLexer i) of
            Bad s -> do putStrLn s
                        loop
            Ok e -> case runTI (infer tEnv (cExp e)) of   --empty env, replace with startTIEnv when implemented
                (Left error,_) -> do
                    putStrLn "TYPE ERROR:"
                    putStrLn $ error ++ " in expression: \n" ++ (show e)
                    case eval env (cExp e) of  --for testing, remove when startTIEnv implemented
                        VIO io   -> putStrLn "running" >> io >> loop  --for testing
                        (VFun _) -> putStrLn "function" >> loop  --for testing
                        v        -> print v >> loop  --for testing
                    --loop
                (Right t,_)    -> case eval env (cExp e) of
                    VIO io   -> putStrLn "running" >> io >> loop
                    (VFun _) -> putStrLn "function" >> loop
                    v        -> print v >> loop


buildEnv :: String -> IO (Env, TypeEnv)
buildEnv ""   = do
    putStrLn "No file loaded"
    return (startEnv, (tiStartEnv (TypeEnv M.empty) startEnvironment))
buildEnv file = do
    res <- try $ readFile (file ++ ".lp")
    case (res :: Either IOError String) of
        Right content -> do
            fc <- readFile (file ++ ".lp")
            prog <- let ts = (myLLexer fc) in case pProgram ts of
                Bad s   -> do putStrLn s
                              throw SyntaxError
                Ok tree -> return tree
            (sEnv, (TypeEnv sTEnv)) <- buildSugar
            let p@(Program ds fs) = cProgram prog
                env   = addFuncDeclsToEnv env fs
                env'  = addDataDeclsToEnv env ds
                env'' = M.union env' sEnv
                (TypeEnv startTiEnv) = tiStartEnv (TypeEnv M.empty) startEnvironment
                (TypeEnv pTEnv) = progToTypeEnv p
                tEnv  = M.union startTiEnv $ M.union pTEnv sTEnv
                tiTypes = checkDecls p (TypeEnv tEnv)
            case getLefts tiTypes of
                [] -> putStrLn $ "\nSuccessfully loaded " ++ file
                s  -> putStrLn $ "Type error: " ++ s
            return (env'', (TypeEnv tEnv))
        Left  err     -> do
            putStrLn "No such file, nothing loaded."
            throw NoSuchFile
        where
            getLefts :: [(String, TI Type)] -> String
            getLefts []         = []
            getLefts ((s,t):xs) = if isLeft' res
                        then "\n" ++ s ++ " : " ++ (replicate (20 - (length s)) ' ') ++ (either show show $ res) ++ getLefts xs
                        else getLefts xs
                        where res = fst (runTI t)

tiStartEnv :: TypeEnv -> [(String, Value, Scheme)] -> TypeEnv
tiStartEnv env [] = env
tiStartEnv env ((a,b,c):xs) = tiStartEnv (add a c env) xs

isRight' :: Either a b -> Bool
isRight' (Right _) = True
isRight' _         = False

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _        = False

buildSugar :: IO (Env, TypeEnv)
buildSugar = do
    sg <- readFile "sugar.lp"
    sugar <- let ts = (myLLexer sg) in case pProgram ts of
        Bad s   -> do putStrLn s
                      throw SyntaxError
        Ok tree -> return tree
    let p@(Program ds fs) = cProgram sugar
        env = addFuncDeclsToEnv env fs
        env' = addDataDeclsToEnv env ds
    return (env', progToTypeEnv p)

checkDecls :: Program -> TypeEnv -> [(String, TI Type)]
checkDecls p t = Prelude.map (checkDecl t) (getDFuncs p)
    where checkDecl :: TypeEnv -> FuncDecl -> (String, TI Type)
          checkDecl te (DFunc id t vs e) = (id, do
                let es = Prelude.map getExp (getRhs p)
                let as = args e
                let linearOK = True--and $ Prelude.map (linCheck t te) (zip as es)
                r <- infer te e'
                case linearOK of
                    False -> error "Linear fail"
                    True  -> unify t r)
                      where e' = Prelude.foldr ELam e vs

linCheck :: Type -> TypeEnv -> ([Pattern], Exp) -> Bool
linCheck t te (ps,e) = let te' = bindLocalVars ps t te
                           lEnv = initLocal M.empty ps
                      in linearCheck lEnv te' e

-- creates a local startenv inits all variables into it
initLocal :: (M.Map Var Int) -> [Pattern] -> (M.Map Var Int)
initLocal env []     = env
initLocal env (p:ps) = case p of
    (PVar v)        -> initLocal (M.insert v 0 env) ps
    (PConstr cid _) -> initLocal (M.insert cid 0 env) ps
    _               -> initLocal env ps


-- returns the inner expression of a case-expression
getExp :: Exp -> Exp
getExp (ECase _ ((p,e):pes)) = getExp e
getExp (ECase _ ((p,e):[]))  = e
getExp e                     = e

args :: Exp -> [[Pattern]]
args (ECase _ ((p,e):pes)) = (p:(args' e)):(concatMap args (snd $ unzip $ pes))
args _                     = []

args' :: Exp -> [Pattern]
args' (ECase _ [])        = []
args' (ECase _ ((p,e):_)) = p:(args' e)
args' _                   = []

-- binds local variables to a type to use when checking linear type rules
bindLocalVars :: [Pattern] -> Type -> TypeEnv -> TypeEnv
bindLocalVars [] _ tEnv             = tEnv
bindLocalVars ((PVar v):ps) t tEnv  = case t of
                        (TFun t1 t2) -> bindLocalVars ps t2 (add v (Scheme [v] t1) tEnv)
                        (TApp t1 t2) -> bindLocalVars ps t2 (add v (Scheme [v] t1) tEnv)
                        t            -> add v (Scheme [v] t) tEnv
bindLocalVars ((PConstr cid ps'):ps) t tEnv  = case t of
                        (TFun t1 t2) -> bindLocalVars ps t2 (bindAll ps' t1 tEnv)
                        (TApp t1 t2) -> bindLocalVars ps t2 (bindAll ps' t1 tEnv)
                        t            -> bindAll ps' t tEnv
bindLocalVars (_:ps) t tEnv = case t of
                        (TFun t1 t2) -> bindLocalVars ps t2 tEnv
                        (TApp t1 t2) -> bindLocalVars ps t2 tEnv
                        t            -> error "internal error"

-- binds all patterns to a definitive type
bindAll :: [Pattern] -> Type -> TypeEnv -> TypeEnv
bindAll [] _ te     = te
bindAll (p:ps) t te = case p of
    (PVar v) -> bindAll ps t (add v (Scheme [v] (TVar "a")) te)
    _        -> te

-- returns all functions in a program
getDFuncs :: Program -> [FuncDecl]
getDFuncs (Program _ ds) = [d | d@(DFunc id t vs e) <- ds]

-- returns all the right hand side expression of a program
getRhs :: Program -> [Exp]
getRhs p = concatMap getRhs' [e | (DFunc id t vs e) <- getDFuncs p]
    where getRhs' e = case e of
            (ECase _ pes) -> [e' | (p,e') <- pes]
            _             -> []
