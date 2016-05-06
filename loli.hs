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
        []     -> repl "" startEnv (TypeEnv M.empty)
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
    return (startEnv, (TypeEnv M.empty))
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
                (TypeEnv pTEnv) = progToTypeEnv p
                tEnv  = M.union pTEnv sTEnv
                tiTypes = checkDecls p (TypeEnv tEnv)
            putStrLn $ "Typecheck correctly " ++ (show $ all isRight' (Prelude.map (fst . runTI) tiTypes))
            --putStrLn $ " " ++ (show $ getDFuncs p)
            putStrLn $ "\nSuccessfully loaded " ++ file
            return (env'', (TypeEnv tEnv))
        Left  err     -> do
            putStrLn "No such file, nothing loaded."
            throw NoSuchFile

isRight' :: Either a b -> Bool
isRight' (Right _) = True
isRight' _         = False

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

checkDecls :: Program -> TypeEnv -> [TI Type]
checkDecls p t = Prelude.map (checkDecl t) (getDFuncs p)
    where checkDecl :: TypeEnv -> FuncDecl -> (TI Type)
          checkDecl te (DFunc id t vs e) = case (runTI $ infer te e') of
                                              (Left err, _) -> error $ "Type error - " ++ err --
                                              (Right r, _)  -> unify t r
                      where e' = Prelude.foldr ELam e vs

-- returns all functions in a program
getDFuncs :: Program -> [FuncDecl]
getDFuncs (Program _ ds) = [d | d@(DFunc id t vs e) <- ds]

-- returns the expression of a funcion
getExps :: Program -> [Exp]
getExps p = [e | (DFunc id t vs e) <- getDFuncs p]

-- returns all the right hand side expression of a program
getRhs :: Program -> [Exp]
getRhs p = concatMap getRhs' (getExps p)
    where getRhs' e = case e of
            (ECase _ pes) -> [e' | (p,e') <- pes]
            _             -> []
