
-- to run file :
-- $ bnfc -m grammar.cf
-- $ runghc -iAST/:grammar/ Converter.hs

module Converter where

import Interpreter
import qualified DataTypes as D
import AbsGrammar
import qualified AbsGrammar as A

cProgram :: A.Program -> D.Program
cProgram (A.PFuncs d p)   = ((cDeclaration d):(cProgram p))
cProgram (A.PLast d)      = ((cDeclaration d):[])
-- cProgram (PImports i p)

-- converts any declaration to a case
cDeclaration :: A.Declaration -> D.Declaration
cDeclaration (A.DFunc (A.Id name) td defs)
                | (countTd td) <= 1 && (length defs) /= 1 = error "multiple declarations of function"
                | (countTd td) <= 1 = (D.DFunc name [] (defToExp (head defs))) -- pattern matching can't arrise
                | otherwise = (D.DFunc name vars (D.ECase (D.EVar (head vars))
                                                          (map (defsToCase (tail vars)) defs)))
     where vars = take ((countTd td)-1) variables -- create variables of the input parameters

defToExp :: A.Def -> D.Exp
defToExp (DDef _ _ e) = (cExp e) -- gets the expression from a def

-- converts a number of definitions to case-tree
-- first matches the first argument to firt input variable then creates following
-- case-trees
defsToCase :: [D.Var] -> A.Def -> D.Pattern
defsToCase vs (A.DDef _ (a:as) e) = (D.Sim (argToPat a) (eCase vs as e))


-- creates cases of pattern matching
eCase :: [D.Var] -> [A.Arg] -> A.Exp -> D.Exp
eCase [] _ e  = (cExp e)
eCase _ [] e  = (cExp e)
eCase (v:vs) (a:as) e = D.ECase (D.EVar v) [(D.Sim (argToPat a) (eCase vs as e)),
                                            (D.Constr "False" [] (D.EVar "fail"))]

-- list of generated variables to introduce in declaration
variables :: [D.Var]
variables = map (("#x"++).show) [1..]

-- counts number of type declarations
countTd :: A.TypeDecls -> Int
countTd (A.STypeDecl _)      = 1
countTd (A.MTypeDecl _ td)   = 1 + countTd td
countTd (A.MLiTypeDecl _ td) = 1 + countTd td

-- converts a definition to a pattern
-- defToPat :: [D.Var] -> A.Def -> D.Pattern
-- defToPat vs (A.DDef (A.Id cid) args e) = D.Mix (map argToPat args) (cExp e)
-- defToPat vs (A.DGuardsDef (A.Id cid) args guards) = undefined
-- defToPat vs (A.DDef (A.Id cid) args e) = (D.Constr cid [] (cExp e))
    -- where vars as = (map argToVar as)

-- ["x","y"] (sum 9 2 = e)

--defToPat (A.DDef (A.Id cid) args e) = (D.Constr cid (vars args) (cExp e))

-- REPLACED BY PASSED
{-- argToVar :: A.Arg -> D.Var
argToVar (A.DArg3 typeId) = case typeId of
    (STypeIdent (TypeId name))    -> name
    (LiTypeIdent (LiTypeId name)) -> name --}

-- converts args to pat in DataTypes.hs
-- where Pat = PLit Lit | PWild | PVar Var
argToPat :: A.Arg -> D.Pat
argToPat (A.DArg3 tid) = case tid of
    A.STypeIdent (A.TypeId name)    -> D.PVar name
    A.LiTypeIdent (A.LiTypeId name) -> D.PVar name
argToPat (A.DArg4 p)   = case p of
    A.P3 pat -> case pat of
        A.Pwild           -> D.PWild
        (A.PId (Id name)) -> D.PVar name
        (A.PLit lit)      -> D.PLit (cLit lit)

    -- A.P1 lp TODO
    -- A.P2 tp TODO

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
cPattern (P2 (TPattern1 ps)) e = case length ps of
    2   -> D.Tup2 (cPattern (ps !! 0) e) (cPattern (ps !! 1) e) (cExp e)
    3   -> D.Tup3 (cPattern (ps !! 0) e) (cPattern (ps !! 1) e) (cPattern (ps !! 2) e) (cExp e)
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

-- (A.EAdd (A.ELiteral (A.LitInt 2)) (A.ELiteral (A.LitInt 5)))])
-- -->
-- EApp (ELam "x" (EBinOp Add (EVar "x") eFour))
            --    (EApp (ELam "x" (EBinOp Add (EVar "x")
            --    eFour)) eSix)

cExp :: A.Exp -> D.Exp
--cExp (A.EAdd e1 e2)         = (D.EApp (D.ELam "x" (D.EBinOp D.Add (D.EVar "x") (cExp e1))) (cExp e2))
cExp (A.EAdd e1 e2)         = (D.EBinOp D.Add (cExp e1) (cExp e2))
cExp (A.EVar (A.Id name))   = (D.EVar name)
cExp (A.ELiteral lit)       = (D.ELit $ cLit lit)
-- cExp (A.ELet vID)       = (D.ELetIn )
cExp (A.EApp e1 e2)         = (D.EApp (cExp e1) (cExp e2))
cExp (A.EAbs (A.Id name) e) = (D.ELam name (cExp e))
cExp (A.ECase e cs)         = (D.ECase (cExp e) (cCase cs))
cExp (EIf e1 e2 e3)         = (D.ECase (cExp e1) [(D.Constr "True" [] (cExp e2)),
                                                  (D.Constr "False" [] (cExp e3))])
cExp (ETuple t) = case t of
    (Tuple2 e1 e2)    -> D.ETup2 (cExp e1) (cExp e2)
    (Tuple3 e1 e2 e3) -> D.ETup3 (cExp e1) (cExp e2) (cExp e3)

cCase :: A.Cases -> [D.Pattern]
cCase A.ECases3             = []
cCase (A.ECases1 p e cs) = cCase (A.ECases2 p e cs)
cCase (A.ECases2 p e cs) = case p of
    (A.P3 p)  -> case p of
        A.Pwild       -> ((D.Wild (cExp e)):[])
        (A.PLit lit)  -> ((D.Simple (cLit lit) (cExp e)):(cCase cs))
    -- (A.P1 lp) ->
    (A.P2 tp)         -> (cPattern (P2 tp) e):(cCase cs)
    -- TODO add support for lists in cases

cTuple :: A.Tuple -> D.Exp
cTuple (Tuple2 e1 e2)    = D.ETup2 (cExp e1) (cExp e2)
cTuple (Tuple3 e1 e2 e3) = D.ETup3 (cExp e1) (cExp e2) (cExp e3)

cGuard :: A.Guards -> D.Exp
cGuard (A.DGuards1 e1 e2 gs) = D.ECase (cExp e2) [(D.Constr "True" [] (cExp e1)),
                                                  (D.Constr "False" [] (cGuard gs))]
cGuard (A.DGuards2 e1 e2 gs) = D.ECase (cExp e2) [(D.Constr "True" [] (cExp e1)),
                                                  (D.Constr "False" [] (cGuard gs))]
cGuard (A.DExpGuard e)       = (cExp e)


-- todo: Convert if-statement to case
