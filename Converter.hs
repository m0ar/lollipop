
-- to run file :
-- $ bnfc -m grammar.cf
-- $ runghc -iAST/:grammar/ Converter.hs

module Converter where

import Interpreter
import qualified DataTypes as D
import AbsGrammar
import qualified AbsGrammar as A
import Data.Map
import qualified Data.Map as M
import Data.Maybe

-- VarEnv maps user definied variables to internal (#x1, #x2, ...)
type VarEnv = Map D.Var D.Var


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
                    = D.DFunc name vars (defsToCase vars vars (asOrg defs) defs)
                | otherwise = D.DFunc name [] (defToExp $ head defs) -- pattern matching can't arrise
     where vars = take (countAs $ head defs) variables -- reserves variables for the input arguments
           nbrAs = countAs (head defs) -- an arbitrary definitions number of arguments
           countAs (A.DDef _ as _) = length as -- counts number of arguments of a definition
           sameNbrAs = all (== nbrAs) (Prelude.map countAs defs) -- all defs should have same number of arguments
           asOrg ds = (\(A.DDef _ as _) -> as) $ head ds

defToExp :: A.Def -> D.Exp
defToExp (A.DDef _ _ e) = cExp e M.empty -- gets the expression from a def

-- converts a number of definitions to case-tree
-- first matches the first argument to firt input variable then creates following
-- case-trees
defsToCase :: [D.Var] -> [D.Var] -> [A.Arg] -> [A.Def] -> D.Exp
defsToCase vsOrg (v:[]) asOrg ((A.DDef _ (a:[]) e):[])  = D.ECase (D.EVar v) [((argToPat a), (cExp e ve))]
    where ve = addManyToVarEnv M.empty (Prelude.map asToVar asOrg) vsOrg
defsToCase vsOrg (v:[]) asOrg ((A.DDef _ (a:[]) e):ds)  = D.ECase (D.EVar v)
                                                    [((argToPat a), (cExp e ve)),
                                                    (D.PWild, (defsToCase vsOrg vsOrg asOrg' ds))]
    where ve = addManyToVarEnv M.empty (Prelude.map asToVar asOrg) vsOrg
          asOrg' = (\(A.DDef _ as _) -> as) $ head ds
defsToCase vsOrg (v:vs) asOrg ((A.DDef did (a:as) e):ds)  = D.ECase (D.EVar v)
                                                    [((argToPat a), (defsToCase vsOrg vs asOrg ((A.DDef did as e):ds))),
                                                     (D.PWild, (defsToCase vsOrg vsOrg asOrg ds))]


asToVar (A.DArg p) = case p of
    A.PPat pat -> case pat of
        (A.PId (Id name)) -> name
        _                 -> "##"    -- not a user defined variable

 -- Adds many variable, value mappings to environment.
-- Used in case and pattern matching
addManyToVarEnv :: VarEnv -> [D.Var] -> [D.Var] -> VarEnv
addManyToVarEnv env   []        []      = env
addManyToVarEnv env   []        _       = error "variables not same length"
addManyToVarEnv env   _        []       = error "variables not same length"
addManyToVarEnv env (v:vs) (v':vs') = addManyToVarEnv (addToVarEnv env v v') vs vs'


 -- Adds a variable, value mapping to environment
addToVarEnv :: VarEnv -> D.Var -> D.Var -> VarEnv
addToVarEnv env v v' = case M.lookup v env of
             Nothing  -> M.insert v v' env
             Just val -> M.insert v v' (M.delete v env)

-- list of generated variables to introduce in declaration
variables :: [D.Var]
variables = Prelude.map (("#x"++).show) [1..]

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
argToPat :: A.Arg -> D.Pattern
argToPat (A.DArg p) = case p of
    -- A.PTuplePat tp -> -- TODO
    -- A.PListPat tp  -> -- TODO
    A.PPat pat     -> case pat of
        A.Pwild           -> D.PWild
        --(A.PId (Id name)) -> D.PVar name
        (A.PId _)         -> D.PWild
        (A.PLit lit)      -> D.PLit (cLit lit)
        --(A.PConst (DConst (TypeId tid) ids)) -> -- TODO

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

cPattern :: A.Pattern -> A.Exp -> VarEnv -> (D.Pattern, D.Exp)
-- cPattern (PListPat lp) e = TODO Add suport for lists
-- cPattern (PTuplePat (TPattern1 ps)) e = case length ps of
--     2   -> D.Tup2 (cPattern (ps !! 0) e) (cPattern (ps !! 1) e) (cExp e)
--     3   -> D.Tup3 (cPattern (ps !! 0) e) (cPattern (ps !! 1) e) (cPattern (ps !! 2) e) (cExp e)
cPattern (PPat p) e ve = cPat p e ve

-- A pattern from bnfc has no expression bound to it
-- this must be sent from the Def to create the AST pattern
cPat :: A.Pat -> A.Exp -> VarEnv -> (D.Pattern, D.Exp)
cPat Pwild           e ve = (D.PWild, (cExp e ve))
cPat (PId (Id name)) e ve = ((D.PVar name), (cExp e ve))
cPat (PLit l)        e ve = ((D.PLit (cLit l)), (cExp e ve))
--cPat (PConst c)      e = -- TODO

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

cExp :: A.Exp -> VarEnv -> D.Exp
--cExp (A.EAdd e1 e2)         = (D.EApp (D.ELam "x" (D.EBinOp D.Add (D.EVar "x") (cExp e1))) (cExp e2))
cExp (A.EAdd e1 e2) ve         = (D.EBinOp D.Add (cExp e1 ve) (cExp e2 ve))
cExp (A.EVar (A.Id name)) ve   = (D.EVar (fromJust $ M.lookup name ve))
                            -- switches user definied variable for internal
cExp (A.ELiteral lit) ve       = (D.ELit $ cLit lit)
-- cExp (A.ELet vID)       = (D.ELetIn )
cExp (A.EApp e1 e2) ve         = (D.EApp (cExp e1 ve) (cExp e2 ve))
cExp (A.EAbs (A.Id name) e) ve = (D.ELam name (cExp e ve))
cExp (A.ECase e cs) ve         = (D.ECase (cExp e ve) (cCase cs ve))
cExp (EIf e1 e2 e3) ve         = (D.ECase (cExp e1 ve) [((D.PConstr "True" []), (cExp e2 ve)),
                                                  ((D.PConstr "False" []), (cExp e3 ve))])
cExp (ETuple t) ve = case t of
    (Tuple2 e1 e2)    -> D.ETup2 (cExp e1 ve) (cExp e2 ve)
    (Tuple3 e1 e2 e3) -> D.ETup3 (cExp e1 ve) (cExp e2 ve) (cExp e3 ve)

cCase :: A.Cases -> VarEnv -> [(D.Pattern, D.Exp)]
cCase A.ECases3          ve = []
cCase (A.ECases1 p e cs) ve = cCase (A.ECases2 p e cs) ve
cCase (A.ECases2 p e cs) ve = (cPattern p e ve):(cCase cs ve)
    -- (A.PListPat lp) ->
    -- (A.PTuplePat tp)         -> (cPattern (P2 tp) e):(cCase cs)
    -- TODO add support for lists in cases

{-- cTuple :: A.Tuple -> D.Exp
cTuple (Tuple2 e1 e2)    = D.ETup2 (cExp e1) (cExp e2)
cTuple (Tuple3 e1 e2 e3) = D.ETup3 (cExp e1) (cExp e2) (cExp e3) --}

cGuard :: A.Guards -> VarEnv -> D.Exp
cGuard (A.DGuards1 e1 e2 gs) ve = D.ECase (cExp e2 ve) [((D.PConstr "True" []), (cExp e1 ve)),
                                                  ((D.PConstr "False" []), (cGuard gs ve))]
cGuard (A.DGuards2 e1 e2 gs) ve = D.ECase (cExp e2 ve) [((D.PConstr "True" []), (cExp e1 ve)),
                                                  ((D.PConstr "False" []), (cGuard gs ve))]
cGuard (A.DExpGuard e) ve       = (cExp e ve)


-- todo: Convert if-statement to case
