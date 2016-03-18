-- To play around a bit with your interpreter
-- I wrote this little front end for the various
-- parts of your program.
-- to run : runghc -iAST/:grammar/ Repl.hs

import System.IO
import Interpreter
import Converter hiding (main)
import DataTypes
import Environment
import qualified AbsGrammar as A

import LexGrammar
import ParGrammar
import qualified AbsGrammar as G
import ErrM

import System.Environment as E

main = do
  E.getArgs >>= \s -> case s of
    [file] -> buildEnv file >>= repl file
    []     -> repl "" startEnv
    _      -> putStrLn "Invalid arguments"

repl :: String -> Env -> IO ()
repl file env = do
        let loop = repl file env
        putStr (file ++ ">") >> hFlush stdout
        i <- getLine
        case i of
         "" -> loop
         ":q" -> return ()
         ":r" -> buildEnv file >>= repl file
         (':':'l':s) -> case words s of
           [newfile] -> buildEnv newfile >>= repl newfile
         _ -> case pExp (myLexer i) of
           Bad s    -> do putStrLn "Syntax error:"
                          putStrLn s
                          loop
           Ok e -> case eval env (cExp e) of -- TODO: type check input
               VIO io -> putStrLn "running" >> io >> loop
               VFun _ -> putStrLn "function" >> loop
               v      -> print v >> loop


buildEnv ""   = do
  putStrLn "No file loaded"
  return startEnv
buildEnv file = do
  fc <- readFile (file++".lp")
  prog <- case pProgram (myLexer fc) of
           Bad s    -> do putStrLn "Parse error!"
                          error s
           Ok  tree -> return tree
  let ds = cProgram prog
      -- TODO: type check ds
      env = addDecsToEnv env ds
  putStrLn $ "Successfully loaded "++file
  return env