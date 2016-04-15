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
cDeclaration (DData (STypeIdent (TypeId s)) fTs dPs) = D.DConstr s (D.VConstr s (map dPatToVal dPs))

dPatToVal :: A.DPatterns -> D.Value
dPatToVal (DDTypes3 id) =
    case id of
        (STypeIdent  (TypeId s)) -> (D.VConstr s []) -- TODO lägg till s som en typ i miljön
        (LiTypeIdent (Id s))     -> (D.VConstr s [])
dPatToVal (DDTypes4 id ft fts) =
    case id of
        (STypeIdent  (TypeId s)) -> (D.VConstr s (map fieldTypeToVal (ft:fts)))
        (LiTypeIdent (Id s))     -> (D.VConstr s (map fieldTypeToVal (ft:fts)))

fieldTypeToVal :: A.FieldType -> D.Value
fieldTypeToVal (FieldType1 (Id s))     = (D.VConstr s [])
fieldTypeToVal (FieldType2 (TypeId s)) = (D.VConstr s [])

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
cGuard (A.DGuards2 e1 e2 gs) = A.ECase e2 (A.ECases2 (A.PConstrEmp (TypeId "True")) e1 (cGuard' gs))
    where
        cGuard' (A.DGuards2 _ _ _) = (A.ECases3 A.PWild (cGuard gs))
        cGuard' (A.DExpGuard e)        = (A.ECases3 A.PWild e)
cGuard (A.DExpGuard e)       = A.ECase e ((A.ECases3 (A.PWild)) e)
                            -- last one is "otherwise"-case

-- list of generated variables to introduce in declaration
variables :: [D.Var]
variables = map (("#x"++).show) [1..]

-- converts args to pat in DataTypes.hs
-- where Pat = PLit Lit | PWild | PVar Var
argToPat :: A.Arg -> D.Pattern
argToPat (A.DArg p) = cPattern p

cPattern :: A.Pattern -> D.Pattern
cPattern p = case p of
    A.PTuplePat p1 p2     -> D.PConstr "(,)" (Prelude.map argToPat $ Prelude.map (\p -> A.DArg p) [p1,p2])
    A.PTruplePat p1 p2 p3 -> D.PConstr "(,,)" (Prelude.map argToPat $ Prelude.map (\p -> A.DArg p) [p1,p2,p3])
    A.PListPat lp         -> cLPat lp
    A.PWild               -> D.PWild
    (A.PId (A.Id name))   -> (D.PVar name)
    (A.PLit l)            -> (D.PLit (cLit l))
    (A.PConstrEmp (TypeId name)) -> (D.PConstr name [])
    (A.PCons p1 p2)       -> D.PConstr "Cons" [(cPattern p1), (cPattern p2)]
    (A.PConsConstr (TypeId name) p1 ps p2) -> D.PConstr "Cons" [(D.PConstr name (map cPattern (p1:ps))), (cPattern p2)]
    A.PEmpty              -> (D.PConstr "Nil" [])

cLPat :: A.ListPat -> D.Pattern
cLPat (PList2 p lp) = D.PConstr "Cons" [(cPattern p), (cLPat lp)]
cLPat (PList1 p )    = D.PConstr "Cons" [(cPattern p), (D.PConstr "Nil" [])]


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
cConst (DConst1 (TypeId cid) cid' ids) = D.EConstr cid
cConst (DConst2 (TypeId cid))         = D.EConstr cid

cExp :: A.Exp -> D.Exp
cExp (A.EVar (A.Id name))   = (D.EVar name)
cExp (A.ETuple t)           = cTuple t
cExp (A.ELiteral lit)       = (D.ELit $ cLit lit)
cExp (A.EConst c)            = case c of
    (A.DConst2 (A.TypeId name)) -> (D.EConstr name)
-- cExp (A.EListComp e lcps)
cExp (A.EList ls)           = cList ls
cExp A.EEmptyList           = D.EConstr "Nil"
cExp (A.ELet (Id n) e1 e2)  = (D.ELetIn n (cExp e1) (cExp e2))
cExp (A.EApp e1 e2)         = (D.EApp (cExp e1) (cExp e2))
cExp (A.ELogicalNeg e)      = (D.EUnOp D.Not (cExp e))
cExp (A.ENeg e)             = (D.EBinOp D.Mul (cExp e) (D.ELit (D.ILit (-1))))
cExp (A.EConcat e1 e2)      = (D.EBinOp D.Concat (cExp e1) (cExp e2))
cExp (A.ECons e1 e2)        = (D.EBinOp D.Cons (cExp e1) (cExp e2))
cExp (A.EPow e1 e2)         = (D.EBinOp D.Pow (cExp e1) (cExp e2))
cExp (A.EMul e1 e2)         = (D.EBinOp D.Mul (cExp e1) (cExp e2))
cExp (A.EDiv e1 e2)         = (D.EBinOp D.Div (cExp e1) (cExp e2))
cExp (A.EAdd e1 e2)         = (D.EBinOp D.Add (cExp e1) (cExp e2))
cExp (A.ESub e1 e2)         = (D.EBinOp D.Add (cExp e1) (cExp (A.ENeg e2)))
cExp (A.ELt e1 e2)          = (D.EBinOp D.Gt (cExp e2) (cExp e1))
cExp (A.EGt e1 e2)          = (D.EBinOp D.Gt (cExp e1) (cExp e2))
cExp (A.ELEq e1 e2)         = (D.EBinOp D.Or (cExp (A.ELt e1 e2)) (cExp (A.EEq e1 e2)))
cExp (A.EGEq e1 e2)         = (D.EBinOp D.Or (cExp (A.EGt e1 e2)) (cExp (A.EEq e1 e2)))
cExp (A.EEq e1 e2)          = (D.EBinOp D.Eq (cExp e1) (cExp e2))
cExp (A.ENEq e1 e2)         = (D.EUnOp D.Not (D.EBinOp D.Eq (cExp e1) (cExp e2)))
cExp (A.EAnd e1 e2)         = D.EUnOp D.Not $ D.EBinOp D.Or (D.EUnOp D.Not (cExp e1)) (D.EUnOp D.Not (cExp e2))
cExp (A.EOr e1 e2)          = (D.EBinOp D.Or (cExp e1) (cExp e2))
cExp (A.EBind e1 e2)        = (D.EBinOp D.Bind (cExp e1) (cExp e2))
cExp (A.Eseq e1 e2)         = (D.EBinOp D.Then (cExp e1) (cExp e2))
cExp (A.ECase e cs)         = (D.ECase (cExp e) (cCase cs))
cExp (A.EIf e1 e2 e3)       = (D.ECase (cExp e1) [((D.PConstr "True" []), (cExp e2)),
                                                  ((D.PConstr "False" []), (cExp e3))])
cExp (A.EAbs (A.Id n) ns e) = (D.ELam n (cList' ns e))
    where cList' ((A.Id n):ns) e = (D.ELam n (cList' ns e))
          cList' []            e = cExp e

{-- cLetIn :: A.LetBinding -> D.Exp
cLetIn (ELetBinding1 ls e) = cLetIn' ls e
    where
        cLetIn' ((ELetBinding2 (Id name) e1):[]) e = D.ELetIn name (cExp e1) (cExp e)
        cLetIn' ((ELetBinding2 (Id name) e1):ls) e = error "Expected one argument in let"
        --cLetIn' ((ELetBinding2 (Id name) e1):ls) e = D.ELetIn name (cExp e1) (cLetIn' ls e)
--}

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

cConstList :: [A.Exp] -> D.Exp
cConstList []              = D.EConstr "Nil"
cConstList ((EConst c):cs) = D.EApp ((D.EApp (D.EConstr "Cons") (cConst c))) (cConstList cs)

cLitList :: [A.Exp] -> D.Exp
cLitList []                = D.EConstr "Nil"
cLitList ((ELiteral l):ls) = D.EApp ((D.EApp (D.EConstr "Cons") (D.ELit $ cLit l))) (cLitList ls)

cCase :: A.Cases -> [(D.Pattern, D.Exp)]
cCase (A.ECases3 p e)    = [((cPattern p),(cExp e))]
cCase (A.ECases1 p e cs) = cCase (A.ECases2 p e cs)
cCase (A.ECases2 p e cs) = ((cPattern p),(cExp e)):(cCase cs)

cTuple :: A.Tuple -> D.Exp
cTuple (Tuple1 e1 e2)    = D.EApp (D.EApp (D.EVar "(,)") (cExp e1)) (cExp e2)
cTuple (Tuple2 e1 e2 e3) = D.EApp (D.EApp (D.EApp (D.EVar "(,,)") (cExp e1)) (cExp e2)) (cExp e3)

{--cGuard :: A.Guards -> A.Exp
cGuard (A.DGuards1 e1 e2 gs) = D.ECase (cExp e2) [((D.PConstr "True" []), (cExp e1)),
                                                  ((D.PConstr "False" []), (cGuard gs))]
cGuard (A.DGuards2 e1 e2 gs) = D.ECase (cExp e2) [((D.PConstr "True" []), (cExp e1)),
                                                  ((D.PConstr "False" []), (cGuard gs))]
cGuard (A.DExpGuard e)       = (cExp e)--}
