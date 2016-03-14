
-- to run file :
-- $ bnfc -m grammar.cf
-- $ runghc -iAST/:grammar/ Converter.hs

module Converter where

import Interpreter
import qualified DataTypes as D
import AbsGrammar
import qualified AbsGrammar as A

main :: IO ()
main = putStrLn ""

convert :: A.Program -> IO ()
convert ds = interpret $ cProgram ds

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

vs = ["a","b","c","d","e","f","g"] -- todo: rename "#x1" "#x2"
                                    -- define inf list instead: map (("#x"++).show) [1..]

defToPat :: A.Def -> D.Pattern
defToPat (A.DDef (A.Id cid) args e) = (D.Constr cid (vars args) (cExp e))
    where vars as = (map argToVar as)
defToPat (A.DGuardsDef (A.Id cid) args guards) = undefined

argToVar :: A.Arg -> D.Var
argToVar (A.DArg3 typeId) = case typeId of
    (STypeIdent (TypeId name))    -> name
    (LiTypeIdent (LiTypeId name)) -> name


getArgs :: [A.Def] -> D.Vars
getArgs = undefined

cArg :: A.Arg -> D.Exp
-- cArg (A.DArg4 (A.P1 lp)) = TODO Implement list-patterns in AST
-- cArg (A.DArg4 (A.P2 (A.TPattern1 ps))) = TODO Implement tuples-patterns in AST
cArg (A.DArg4 (A.P3 (A.PLit lit))) = case lit of
    (A.LitInt x)    -> D.ELit (D.ILit (fromInteger x))
    (A.LitDouble x) -> D.ELit (D.DLit x)
    (A.LitChar x)   -> D.ELit (D.CLit x)
cArg (A.DArg3 typeId) = case typeId of
    (A.STypeIdent (TypeId name))    -> D.EVar name
    (A.LiTypeIdent (LiTypeId name)) -> D.EVar name

cPattern :: A.Pattern -> A.Exp -> D.Pattern
-- cPattern (P1 lp) e = TODO Add suport for lists
-- cPattern (P2 tp) e = TODO Add support for tuples
cPattern (P3 p) e = (cPat p e)

-- A pattern from bnfc has no expression bound to it
-- this must be sent from the Def to create the AST pattern
cPat :: A.Pat -> A.Exp -> D.Pattern
cPat Pwild e           = D.Wild (cExp e)
cPat (PId (Id name)) e = D.Variable name (cExp e)
cPat (PLit l)        e = D.Simple (cLit l) (cExp e)

cType :: A.Type -> D.Exp
cType (TTypeId t) = case t of -- TODO check this part
    (STypeIdent (TypeId name))    -> (D.EVar name)
    (LiTypeIdent (LiTypeId name)) -> (D.EVar name)
-- cType (TPoly ti)   = what is poly?
cType (TList ts)  = cList ts
    where cList []     = D.EConstr "Nil"
          cList (t:ts) = D.EApp (D.EApp (D.EConstr "Cons") (cType t)) (cList ts)

cLit :: A.Literal -> D.Lit
cLit (A.LitInt x)      = D.ILit $ fromInteger x
cLit (A.LitDouble x)   = D.DLit x
cLit (A.LitChar x)     = D.CLit x
cLit (A.LitString x)   = D.SLit x

cExp :: A.Exp -> D.Exp
cExp (A.EVar (A.Id name))   = (D.EVar name)
cExp (A.ELiteral lit)       = (D.ELit $ cLit lit)
-- cExp (A.ELet vID)       = (D.ELetIn )
cExp (A.EApp e1 e2)         = (D.EApp (cExp e1) (cExp e2))
cExp (A.EAbs (A.Id name) e) = (D.ELam name (cExp e))
cExp (A.ECase e cs)         = (D.ECase (cExp e) (cCase cs))
cExp (EIf e1 e2 e3)         = (D.ECase (cExp e1) [(D.Constr "True" [] (cExp e2)),
                                                  (D.Constr "False" [] (cExp e3))])

cCase :: A.Cases -> [D.Pattern]
cCase A.ECases3             = []
cCase (A.ECases1 p e cs) = cCase (A.ECases2 p e cs)
cCase (A.ECases2 p e cs) = case p of
    (A.P3 p)  -> case p of
        A.Pwild       -> ((D.Wild (cExp e)):[])
        (A.PLit lit)  -> ((D.Simple (cLit lit) (cExp e)):(cCase cs))
    -- (A.P1 lp) ->
    -- (A.P2 tp) ->
    -- TODO add support for tuples and lists in cases



cGuard :: A.Guards -> D.Exp
cGuard (A.DGuards1 e1 e2 gs) = D.ECase (cExp e2) [(D.Constr "True" [] (cExp e1)),
                                                  (D.Constr "False" [] (cGuard gs))]
cGuard (A.DGuards2 e1 e2 gs) = D.ECase (cExp e2) [(D.Constr "True" [] (cExp e1)),
                                                  (D.Constr "False" [] (cGuard gs))]
cGuard (A.DExpGuard e)       = (cExp e)


-- todo: Convert if-statement to case
