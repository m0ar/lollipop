
-- to run file :
-- $ bnfc -m grammar.cf
-- $ runghc -iAST/:grammar/ Converter.hs

module Converter where

import Frame
import qualified DataTypes as D
import AbsGrammar
import qualified AbsGrammar as A

main :: IO ()
main = putStrLn "Velkommen til oversetteren fra BNFC AST til lollipop AST!"

cProgram :: A.Program -> D.Program
-- cProgram (PImports i p)
cProgram (A.PFuncs d p)   = ((cDeclaration d):(cProgram p))
cProgram (A.PLast d)      = ((cDeclaration d):[])

-- converts any declaration to a case
cDeclaration :: A.Declaration -> D.Declaration
cDeclaration = undefined
--cDeclaration (A.DFunc (A.Id name) ds defs) = (D.DFunc name vars eCase)
    -- where eCase = (D.ECase (D.EVar "placeholder") (map defToPat defs))
          -- vars = take ((length ds)-1) vs -- create variables of the input parameters

vs = ["a","b","c","d","e","f","g"]

defToPat :: A.Def -> D.Pattern
defToPat (A.DDef (A.Id cid) args e) = (D.Constr cid (vars args) (cExp e))
    where vars as = (map argToVar as)
defToPat (A.DGuardsDef (A.Id cid) args guards) = undefined

argToVar :: A.Arg -> D.Var
argToVar (A.DArg2 (A.Id name)) = name


getArgs :: [A.Def] -> D.Vars
getArgs = undefined

cArg :: A.Arg -> D.Exp
cArg (A.DArg1 lit) = case lit of
    (A.LitInt x)    -> D.ELit (D.ILit (fromInteger x))
    (A.LitDouble x) -> D.ELit (D.DLit x)
    (A.LitChar x)   -> D.ELit (D.CLit x)
cArg (A.DArg2 (A.Id name)) = D.EVar name
cArg (A.DArg3 t) = case t of
    (A.TTypeId (TypeId name))     -> D.EConstr name
    (A.TLiTypeId (LiTypeId name)) -> D.EConstr name
    -- (LitString x) ->

cLit :: A.Literal -> D.Lit
cLit (A.LitInt x)      = D.ILit $ fromInteger x
cLit (A.LitDouble x)   = D.DLit x
cLit (A.LitChar x)     = D.CLit x
cLit (A.LitString x)   = D.SLit x

cExp :: A.Exp -> D.Exp
cExp (A.EVar (A.Id name))   = (D.EVar name)
cExp (A.ELiteral lit)     = (D.ELit $ cLit lit)
-- cExp (A.ELet vID)       = (D.ELetIn )
cExp (A.EApp e1 e2)       = (D.EApp (cExp e1) (cExp e2))
cExp (A.EAdd e1 e2)       = (D.EAdd (cExp e1) (cExp e2))
cExp (A.EAbs (A.Id name) e) = (D.ELam name (cExp e))

cGuard :: A.Guards -> D.Exp
cGuard (A.DGuards1 e1 e2 gs) = D.ECase (cExp e2) [(D.Constr "True" [] (cExp e1)),
                                                  (D.Constr "False" [] (cGuard gs))]
cGuard (A.DGuards2 e1 e2 gs) = D.ECase (cExp e2) [(D.Constr "True" [] (cExp e1)),
                                                  (D.Constr "False" [] (cGuard gs))]
cGuard (A.DExpGuard e)       = (cExp e)


-- todo: Convert if-statement to case
