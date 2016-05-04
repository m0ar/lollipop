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

import LexGrammar
import ParGrammar
import LayoutGrammar
import qualified AbsGrammar as G
import ErrM
import Control.Exception
import Data.Typeable

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
    case i of
        "" -> loop
        ":q" -> return ()
        ":r" -> buildEnv file >>= uncurry (repl file)
        (':':'t':' ':s) -> putStrLn (show (lookupInEnv env s))
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
            tiTypes <- checkDecls p (TypeEnv sTEnv)
            putStrLn $ "Successfully loaded " ++ file
            return (env'', (TypeEnv tEnv))
        Left  err     -> do
            putStrLn "No such file, nothing loaded."
            throw NoSuchFile

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

checkDecls :: Program -> TypeEnv -> IO [TI Type]
checkDecls p t = return $ Prelude.map (checkDecl t) (getDFuncs p)
    where checkDecl :: TypeEnv -> FuncDecl -> TI Type
          checkDecl te (DFunc id t vs e) = case (runTI $ infer te e) of
              (Left err, _) -> error (show err)
              (Right r, _)  -> unify t r

getDFuncs :: Program -> [FuncDecl]
getDFuncs (Program _ ds) = [d | d@(DFunc id t vs e) <- ds]
