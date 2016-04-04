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

-- converts args to pat in DataTypes.hs
-- where Pat = PLit Lit | PWild | PVar Var
argToPat :: A.Arg -> D.Pattern
argToPat (A.DArg p) = case p of
    A.PTuplePat (A.TPattern ps) -> case length ps of
        2 -> D.PConstr "(,)" (Prelude.map argToPat $ Prelude.map (\p -> A.DArg p) ps)
        3 -> D.PConstr "(,,)" (Prelude.map argToPat $ Prelude.map (\p -> A.DArg p) ps)
    A.PListPat lp  -> case lp of
        LPattern2 p lp' -> cLPat (LPattern5 p lp')
        LPattern1       -> cLPat LPattern3
    A.PPat pat     -> case pat of
        A.Pwild           -> D.PWild
        (A.PId (Id name)) -> D.PVar name
        (A.PLit lit)      -> D.PLit (cLit lit)
        (A.PConst (DConst (TypeId name) _ _)) -> D.PConstr name []
        (A.PConst (DConst1 (TypeId name)))    -> D.PConstr name []

cLPat :: A.LPattern -> D.Pattern
cLPat LPattern3                = D.PConstr "Nil" []
cLPat (LPattern4 p)            = D.PConstr "Cons" [(cPat p), (cLPat LPattern3)]
cLPat (LPattern5 (PPat p) lps) = D.PConstr "Cons" [(cPat p), (cLPat lps)]

cPattern :: A.Pattern -> A.Exp -> (D.Pattern, D.Exp)
cPattern l@(PListPat _) e     = (lPat, (cExp e))
    where lPat = argToPat (A.DArg l)
cPattern p@(A.PTuplePat _) e = (tPat, (cExp e))
    where tPat = argToPat (A.DArg p)
cPattern (PPat p) e          = ((cPat p), (cExp e))

-- A pattern from bnfc has no expression bound to it
-- this must be sent from the Def to create the AST pattern
cPat :: A.Pat -> D.Pattern
cPat A.Pwild             = D.PWild
cPat (A.PId (A.Id name)) = (D.PVar name)
cPat (A.PLit l)          = (D.PLit (cLit l))
cPat (A.PConst c)        = case c of
    A.DConst1 (A.TypeId bool)       -> (D.PConstr bool [])
    A.DConst (A.TypeId bool) id ids -> (D.PConstr bool [])

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

cConst :: A.Cons -> D.Exp
cConst (DConst (TypeId cid) cid' ids) = D.EConstr cid
cConst (DConst1 (TypeId cid))         = D.EConstr cid

cExp :: A.Exp -> D.Exp
cExp (A.EVar (A.Id name))   = (D.EVar name)
cExp (A.ETuple t)           = cTuple t
cExp (A.ELiteral lit)       = (D.ELit $ cLit lit)
cExp (A.EConst c)            = case c of
    (A.DConst1 (A.TypeId bool)) -> (D.EConstr bool)
    -- DConst (TypeId bool) id ids -> -- TODO
-- cExp (A.EListComp e lcps)
cExp (A.EList ls)           = cList ls
cExp A.EEmptyList           = D.EConstr "Nil"
-- cExp (A.ELet vID)       = (D.ELetIn )
cExp (A.EApp e1 e2)         = (D.EApp (cExp e1) (cExp e2))
-- cExp (A.ELogicalNeg e)
cExp (A.ENeg e)             = (D.EBinOp D.Mul (cExp e) (D.ELit (D.ILit (-1))))
-- cExp (A.EPow e1 e2)
cExp (A.EMul e1 e2)         = (D.EBinOp D.Mul (cExp e1) (cExp e2))
cExp (A.EDiv e1 e2)         = (D.EBinOp D.Div (cExp e1) (cExp e2))
cExp (A.EAdd e1 e2)         = (D.EBinOp D.Add (cExp e1) (cExp e2))
cExp (A.ESub e1 e2)         = (D.EBinOp D.Sub (cExp e1) (cExp e2))
cExp (A.ELt e1 e2)          = (D.EBinOp D.Gt (cExp e2) (cExp e1))
cExp (A.EGt e1 e2)          = (D.EBinOp D.Gt (cExp e1) (cExp e2))
cExp (A.ELEQ e1 e2)         = (D.EBinOp D.Or (cExp (A.ELt e1 e2)) (cExp (A.EEQ e1 e2)))
cExp (A.EGEQ e1 e2)         = (D.EBinOp D.Or (cExp (A.EGt e1 e2)) (cExp (A.EEQ e1 e2)))
cExp (A.EEQ e1 e2)          = (D.EBinOp D.Eq (cExp e1) (cExp e2))
cExp (A.ENEQ e1 e2)         = (D.EUnOp D.Not (D.EBinOp D.Eq (cExp e1) (cExp e2)))
cExp (A.EAND e1 e2)         = D.EUnOp D.Not $ D.EBinOp D.Or (D.EUnOp D.Not (cExp e1)) (D.EUnOp D.Not (cExp e2))
cExp (A.EOR e1 e2)          = (D.EBinOp D.Or (cExp e1) (cExp e2))
cExp (A.EBind e1 e2)        = (D.EBinOp D.Bind (cExp e1) (cExp e2))
cExp (A.Eseq e1 e2)         = (D.EBinOp D.Then (cExp e1) (cExp e2))
cExp (A.ECase e cs)         = (D.ECase (cExp e) (cCase cs))
cExp (A.EIf e1 e2 e3)       = (D.ECase (cExp e1) [((D.PConstr "True" []), (cExp e2)),
                                                  ((D.PConstr "False" []), (cExp e3))])
cExp (A.EAbs (A.Id name) e) = (D.ELam name (cExp e))


cList :: [A.Exp] -> D.Exp
cList ls = case (head ls) of
    ELiteral _ -> cLitList ls
    ETuple _   -> cTupleList ls
    EConst _   -> cConstList ls
    EList _    -> cListList ls

cListList :: [A.Exp] -> D.Exp
cListList []           = D.EConstr "Nil"
cListList (l:ls) = D.EApp ((D.EApp (D.EConstr "Cons") (cExp l))) (cListList ls)

cTupleList :: [A.Exp] -> D.Exp
cTupleList []     = D.EConstr "Nil"
cTupleList ((ETuple t):ts) = D.EApp ((D.EApp (D.EConstr "Cons") (cTuple t))) (cTupleList ts)

-- [ETuple (Tuple2 (ELiteral (LitInt 1)) (ELiteral (LitInt 2))),
 -- ETuple (Tuple2 (ELiteral (LitInt 3)) (ELiteral (LitInt 4)))]

cConstList :: [A.Exp] -> D.Exp
cConstList []              = D.EConstr "Nil"
cConstList ((EConst c):cs) = D.EApp ((D.EApp (D.EConstr "Cons") (cConst c))) (cConstList cs)

cLitList :: [A.Exp] -> D.Exp
cLitList []                = D.EConstr "Nil"
cLitList ((ELiteral l):ls) = D.EApp ((D.EApp (D.EConstr "Cons") (D.ELit $ cLit l))) (cLitList ls)

cCase :: A.Cases -> [(D.Pattern, D.Exp)]
cCase A.ECases3          = []
cCase (A.ECases1 p e cs) = cCase (A.ECases2 p e cs)
cCase (A.ECases2 p e cs) = (cPattern p e):(cCase cs)

cTuple :: A.Tuple -> D.Exp
cTuple (Tuple2 e1 e2)    = D.EApp (D.EApp (D.EVar "(,)") (cExp e1)) (cExp e2)
cTuple (Tuple3 e1 e2 e3) = D.EApp (D.EApp (D.EApp (D.EVar "(,,)") (cExp e1)) (cExp e2)) (cExp e3)

{--cGuard :: A.Guards -> A.Exp
cGuard (A.DGuards1 e1 e2 gs) = D.ECase (cExp e2) [((D.PConstr "True" []), (cExp e1)),
                                                  ((D.PConstr "False" []), (cGuard gs))]
cGuard (A.DGuards2 e1 e2 gs) = D.ECase (cExp e2) [((D.PConstr "True" []), (cExp e1)),
                                                  ((D.PConstr "False" []), (cGuard gs))]
cGuard (A.DExpGuard e)       = (cExp e)--}
