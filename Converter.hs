
-- to run file :
-- $ bnfc -m grammar.cf
-- $ runghc -iAST/:grammar/ Converter.hs

module Converter where

import Interpreter
import qualified DataTypes as D
import AbsGrammar
import qualified AbsGrammar as A

main :: IO ()
main = putStrLn "welcome to the converter"

cProgram :: A.Program -> D.Program
cProgram (A.PFuncs d p)   = ((cDeclaration d):(cProgram p))
cProgram (A.PLast d)      = ((cDeclaration d):[])
-- cProgram (PImports i p)

-- converts any declaration to a case
cDeclaration :: A.Declaration -> D.Declaration
cDeclaration (A.DFunc (A.Id name) _ defs)
                | not sameNbrAs -- definitons has different number of arguments
                    = error $ "Defintions for function " ++ name ++ " have different number of arguments"
                | nbrAs == 0 && length defs > 1 -- if there is no input arguments, but several defs
                    = error $ "Conflicting definitions for function " ++ name
                | nbrAs >= 1 -- pattern matching can arrise
                    = D.DFunc name vars (defsToCase vars vars defs')
                | otherwise = D.DFunc name [] (defToExp $ head defs) -- pattern matching can't arrise
     where vars = take (countAs $ head defs) variables -- reserves variables for the input arguments
           nbrAs = countAs $ head defs -- an arbitrary definitions number of arguments
           countAs (A.DDef _ as _) = length as -- counts number of arguments of a definition
           countAs (A.DGuardsDef _ as _) = length as -- counts number of arguments of a definition
           sameNbrAs = all (== nbrAs) (map countAs defs) -- all defs should have same number of arguments
           defs' = allDef defs

-- extracts the expression from a def
defToExp :: A.Def -> D.Exp
defToExp (A.DDef _ _ e)      = cExp e
defToExp (DGuardsDef _ _ gs) = cExp $ cGuard gs
-- converts a number of definitions to case-tree
-- first matches the first argument to firt input variable then creates following
-- case-trees
defsToCase :: [D.Var] -> [D.Var] -> [A.Def] -> D.Exp
defsToCase  _    (v:[]) ((A.DDef _ (a:[]) e):[])   = D.ECase (D.EVar v) [((argToPat a), (cExp e))]
defsToCase vsOrg (v:[]) ((A.DDef _ (a:[]) e):ds)   = D.ECase (D.EVar v)
                                                      [ ((argToPat a), (cExp e)),
                                                        (D.PWild, (defsToCase vsOrg vsOrg ds))]
defsToCase vsOrg (v:vs) ((A.DDef did (a:as) e):ds) = D.ECase (D.EVar v)
                                                      [ ((argToPat a), (defsToCase vsOrg vs ((A.DDef did as e):ds))),
                                                        (D.PWild, (defsToCase vsOrg vsOrg ds))]

-- translates all definitions into DDef-definitions
allDef :: [A.Def] -> [A.Def]
allDef []     = []
allDef (d:ds) = case d of
    (A.DDef _ _ _)         -> d:(allDef ds)
    (A.DGuardsDef did as gs) -> (A.DDef did as (cGuard gs)):(allDef ds)

-- translates guards into equal case-expressions
cGuard :: A.Guards -> A.Exp
cGuard (A.DGuards1 e1 e2 gs) = cGuard (A.DGuards2 e1 e2 gs)
cGuard (A.DGuards2 e1 e2 gs) = A.ECase e2 (A.ECases2 (A.PPat (A.PConst (A.DConst1 (A.TypeId "True")))) e1 (cGuard' gs))
    where cGuard' (A.DGuards2 e1 e2 gs) = (A.ECases2 (A.PPat (A.PConst (A.DConst1 (A.TypeId "False")))) (cGuard gs) A.ECases3)
cGuard (A.DExpGuard e)       = A.ECase e ((A.ECases2 (A.PPat (A.Pwild))) e (A.ECases3))
                            -- last one is "otherwise"-case

-- list of generated variables to introduce in declaration
variables :: [D.Var]
variables = map (("#x"++).show) [1..]

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
argToPat :: A.Arg -> D.Pattern
argToPat (A.DArg p) = case p of
    -- A.PTuplePat tp -> -- TODO
    -- A.PListPat tp  -> -- TODO
    A.PPat pat     -> case pat of
        A.Pwild           -> D.PWild
        (A.PId (Id name)) -> D.PVar name
        (A.PLit lit)      -> D.PLit (cLit lit)
        (A.PConst (DConst (TypeId name) _ _)) -> D.PConstr name []
        --(A.PConst (DConst (TypeId name) _ _)) -> D.PVar name
        (A.PConst (DConst1 (TypeId name)))    -> D.PConstr name []
        --(A.PConst (DConst1 (TypeId name)))    -> D.PVar name

    -- A.P1 lp TODO
    -- A.P2 tp TODO

{-- cArg :: A.Arg -> D.Exp
-- cArg (A.DArg4 (A.P1 lp)) = TODO Implement list-patterns in AST
-- cArg (A.DArg4 (A.P2 (A.TPattern1 ps))) = TODO Implement tuples-patterns in AST
cArg (A.DArg4 (A.P3 (A.PLit lit))) = case lit of
    (A.LitInt x)    -> D.ELit (D.ILit (fromInteger x))
    (A.LitDouble x) -> D.ELit (D.DLit x)
    (A.LitChar x)   -> D.ELit (D.CLit x)
cArg (A.DArg3 typeId) = case typeId of
    (A.STypeIdent (TypeId name))    -> D.EVar name
    (A.LiTypeIdent (LiTypeId name)) -> D.EVar name --}

cPattern :: A.Pattern -> A.Exp -> (D.Pattern, D.Exp)
-- cPattern (PListPat lp) e = TODO Add suport for lists
-- cPattern (PTuplePat (TPattern1 ps)) e = case length ps of
--     2   -> D.Tup2 (cPattern (ps !! 0) e) (cPattern (ps !! 1) e) (cExp e)
--     3   -> D.Tup3 (cPattern (ps !! 0) e) (cPattern (ps !! 1) e) (cPattern (ps !! 2) e) (cExp e)
cPattern (PPat p) e = cPat p e

-- A pattern from bnfc has no expression bound to it
-- this must be sent from the Def to create the AST pattern
cPat :: A.Pat -> A.Exp -> (D.Pattern, D.Exp)
cPat A.Pwild           e = (D.PWild, (cExp e))
cPat (A.PId (A.Id name)) e = ((D.PVar name), (cExp e))
cPat (A.PLit l)        e = ((D.PLit (cLit l)), (cExp e))
cPat (A.PConst c)      e = case c of
    A.DConst1 (A.TypeId bool)       -> ((D.PConstr bool []), (cExp e))
    A.DConst (A.TypeId bool) id ids -> ((D.PConstr bool []), (cExp e))

cType :: A.Type -> D.Exp
cType (A.TTypeId t) = case t of -- TODO check this part
    (A.STypeIdent (A.TypeId name)) -> (D.EVar name)
    (A.LiTypeIdent (A.Id name))    -> (D.EVar name)
-- cType (TPoly ti)   = what is poly?
cType (A.TList ts)  = cList ts
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
cExp (A.EConst c)            = case c of
    (A.DConst1 (A.TypeId bool)) -> (D.EConstr bool)
    -- DConst (TypeId bool) id ids -> -- TODO
cExp (A.EAdd e1 e2)         = (D.EBinOp D.Add (cExp e1) (cExp e2))
cExp (A.EVar (A.Id name))   = (D.EVar name)
cExp (A.ELiteral lit)       = (D.ELit $ cLit lit)
-- cExp (A.ELet vID)       = (D.ELetIn )
cExp (A.EApp e1 e2)         = (D.EApp (cExp e1) (cExp e2))
cExp (A.EAbs (A.Id name) e) = (D.ELam name (cExp e))
cExp (A.ECase e cs)         = (D.ECase (cExp e) (cCase cs))
cExp (A.EIf e1 e2 e3)         = (D.ECase (cExp e1) [((D.PConstr "True" []), (cExp e2)),
                                                  ((D.PConstr "False" []), (cExp e3))])
cExp (A.ETuple t) = case t of
    (A.Tuple2 e1 e2)    -> D.EApp (D.EApp (D.EVar "(,)") (cExp e1)) (cExp e2)
    --(A.Tuple3 e1 e2 e3) ->

cCase :: A.Cases -> [(D.Pattern, D.Exp)]
cCase A.ECases3          = []
cCase (A.ECases1 p e cs) = cCase (A.ECases2 p e cs)
cCase (A.ECases2 p e cs) = (cPattern p e):(cCase cs)
    -- (A.PListPat lp) ->
    -- (A.PTuplePat tp)         -> (cPattern (P2 tp) e):(cCase cs)
    -- TODO add support for lists in cases

cTuple :: A.Tuple -> D.Exp
cTuple (Tuple2 e1 e2)    = D.ETup2 (cExp e1) (cExp e2)
cTuple (Tuple3 e1 e2 e3) = D.ETup3 (cExp e1) (cExp e2) (cExp e3)

{--cGuard :: A.Guards -> A.Exp
cGuard (A.DGuards1 e1 e2 gs) = D.ECase (cExp e2) [((D.PConstr "True" []), (cExp e1)),
                                                  ((D.PConstr "False" []), (cGuard gs))]
cGuard (A.DGuards2 e1 e2 gs) = D.ECase (cExp e2) [((D.PConstr "True" []), (cExp e1)),
                                                  ((D.PConstr "False" []), (cGuard gs))]
cGuard (A.DExpGuard e)       = (cExp e)--}



-- todo: Convert if-statement to case
