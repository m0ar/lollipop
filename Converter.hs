-- to run file :
-- $ bnfc -m grammar.cf
-- $ runghc -iAST/:grammar/ Converter.hs

module Converter where

import qualified AST.DataTypes as D
import TI
import AbsGrammar
import qualified AbsGrammar as A

main :: IO ()
main = putStrLn "welcome to the converter"

cProgram :: A.Program -> D.Program
cProgram p = D.Program [cDataDecl d | d@(A.DData tId ids cs) <- ds] -- ++[cTypeDecl td | td@(A.DSyn tId ids t d) <- ds]
                     [cFuncDecl f | f@(A.DFunc fId t ds) <- ds]
    where ds = progToDecls p

-- Recursively converts the program to internal syntax by
-- repeatedly applying cDeclaration to each declaration
progToDecls :: A.Program -> [A.Declaration]
progToDecls (A.PFuncs d p)   = d:(progToDecls p)
progToDecls (A.PLast d)      = [d]

-- Converts a function declaration to a case-expression in DataTypes
cFuncDecl :: A.Declaration -> D.FuncDecl
cFuncDecl (A.DFunc (A.Id name) tDecls defs)
                | not sameNbrAs -- definitons has different number of arguments
                    = error $ "Defintions for function " ++ name ++ " have different number of arguments"
                | nbrAs == 0 && length defs > 1 -- if there is no input arguments, but several defs
                    = error $ "Conflicting definitions for function " ++ name
                | nbrAs >= 1 -- pattern matching can arrise
                    = D.DFunc name (cType tDecls) linVs (defsToCase linVs linVs defs')
                | otherwise = D.DFunc name (cType tDecls) [] (defToExp $ head defs) -- pattern matching can't arrise
     where vars  = take (countAs $ head defs) variables -- reserves variables for the input arguments
           linVs = linearize vars (typeDeclToList tDecls) -- creates linear variables
           nbrAs = countAs $ head defs -- an arbitrary definitions number of arguments
           countAs (A.DDef _ as _) = length as -- counts number of arguments of a definition
           countAs (A.DGuardsDef _ as _) = length as -- counts number of arguments of a definition
           sameNbrAs = all (== nbrAs) (map countAs defs) -- all defs should have same number of arguments
           defs' = allDef defs

-- converts the recurive datatype Type to a list of
-- TypeIdents
typeDeclToList :: A.Type -> [A.TypeIdent]
typeDeclToList (TypeIds ti)       = [ti]
typeDeclToList (TypeTuple t ts)   = concatMap typeDeclToList (t:ts)
typeDeclToList (TypeList t)       = typeDeclToList t
typeDeclToList TypeVoid           = []
typeDeclToList (TypeDecl t1 t2)   = (typeDeclToList t1)++(typeDeclToList t2)
typeDeclToList (LiTypeDecl t1 t2) = (typeDeclToList t1)++(typeDeclToList t2)
typeDeclToList (TypeApp t1 t2)    = (typeDeclToList t1)++(typeDeclToList t2)

-- linearizes linear variables for use in interpreter
linearize :: [D.Var] -> [A.TypeIdent] -> [D.Var]
linearize [] _  = []
linearize vs [] = vs
linearize (v:vs) (t:ts) = case t of
    STypeIdent (TypeId name)  -> v:(linearize vs ts)
    LiTypeIdent (Id ('i':_)) -> ('i':v):(linearize vs ts)
    LiTypeIdent (Id name)     -> v:(linearize vs ts)

-- Converts a data declaration to a DataDecl in DataTypes
cDataDecl :: A.Declaration -> D.DataDecl
cDataDecl (DData (STypeIdent (TypeId s)) ids cs) = D.DData s [name | Id name <- ids] (map cConstr cs)

-- Converts a type declaration to a DataDecl in DataTypes
-- cTypeDecl :: A.Declaration -> D.DataDecl
-- cTypeDecl (DSyn (STypeIdent (TypeId s)) ids t d) = D.DData s [name | Id name <- ids] (map cConstr cs)

-- Converts a constructor
cConstr :: A.Constr -> D.ConstrDecl
cConstr (DConstr1 tId tps) =
    case tId of
        (STypeIdent  (TypeId s)) -> D.ConstrDecl s [cType t | (TParameter t) <- tps] -- TODO lägg till s som en typ i miljön
        (LiTypeIdent (Id s))     -> D.ConstrDecl s [cType t | (TParameter t) <- tps]

typeToVal :: A.Type -> D.Value -- TODO
typeToVal t = case t of
        TypeIds (STypeIdent (TypeId t1))    -> D.VConstr t1 []
        TypeIds (LiTypeIdent (Id t1))       -> undefined -- TODO
        TypeTuple t1 t2                     -> case length t2 of
           1 -> D.VConstr "(,)"  [typeToVal $ t1, (typeToVal $ head t2)]
           2 -> D.VConstr "(,,)" [typeToVal t1, (typeToVal $ head t2), (typeToVal $ head $ drop 1 $ t2)]
        -- TypeList, this is now a TConstr "[]", fix accordingly
        TypeVoid                            -> undefined
        TypeDecl t1 t2                      -> undefined
        LiTypeDecl t1 t2                    -> undefined
        TypeApp t1 t2                       -> undefined

-- Converts types from the surface syntax to a minimized set of types used by the typechecker.
cType :: A.Type -> D.Type
cType (A.TypeIds (A.STypeIdent s))  = D.TConstr $ extractTId s
cType (A.TypeIds (A.LiTypeIdent s)) = D.TVar $ extractId s
cType (A.TypeTuple  t1  ts) = case ts of
    (t2:[])    -> D.TApp (D.TApp (D.TConstr "(,)") (cType t1)) (cType t2)
    (t2:t3:[]) -> D.TApp (D.TApp (D.TApp (D.TConstr "(,)") (cType t1)) (cType t2)) (cType t3)
    _          -> error "Tuples only defined for two or three elements."
cType (A.TypeList   t     ) = D.TApp (D.TConstr "[]") (cType t)
cType  A.TypeVoid           = D.TConstr "()"
cType (A.TypeDecl   t1  t2) = D.TFun (cType t1) (cType t2)
cType (A.LiTypeDecl t1  t2) = D.TFun (cType t1) (cType t2)
cType (A.TypeApp    t1  t2) = D.TApp (cType t1) (cType t2)

-- Converts type indentifiers to actual strings
identToString :: A.TypeIdent -> String
identToString (A.STypeIdent s)  = extractTId s
identToString (A.LiTypeIdent s) = extractId s

-- Extracts the string from a TypeId / Id
extractTId :: A.TypeId -> String
extractTId (A.TypeId s) = s
extractId :: A.Id -> String
extractId (A.Id s)      = s


-- Extracts the expression from a def
defToExp :: A.Def -> D.Exp
defToExp (A.DDef _ _ e)         = cExp e
defToExp (A.DGuardsDef _ _ gs)  = cExp $ cGuard gs

-- converts a number of definitions to case-tree
-- first matches the first argument to firt input variable then
-- creates following case-trees

defsToCase :: [D.Var] -> [D.Var] -> [A.Def] -> D.Exp
defsToCase  _    (v:[]) ((A.DDef _ (a:[]) e):[])
    = D.ECase (D.EVar v) [((argToPat a), (cExp e))]
defsToCase  vsOrg (v:vs) ((A.DDef did (a:as) e):[])
    = D.ECase (D.EVar v) [((argToPat a),
                           (defsToCase vsOrg vs [(A.DDef did as e)]))]
defsToCase vsOrg (v:[]) ((A.DDef _ (a:[]) e):ds)
    = D.ECase (D.EVar v)
      [ ((argToPat a), (cExp e)),
        (D.PWild, (defsToCase vsOrg vsOrg ds))]
defsToCase vsOrg (v:vs) ((A.DDef did (a:as) e):ds) =
    D.ECase (D.EVar v)
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
cGuard (A.DGuards2 e1 e2 gs) =
    A.ECase e2 (A.ECases2 (A.PConstrEmp (A.TypeId "True")) e1 (cGuard' gs))
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

typeIdentName :: A.TypeIdent -> String
typeIdentName (STypeIdent (TypeId name)) = name
typeIdentName (LiTypeIdent (Id name))    = name

-- Converts a pattern
cPattern :: A.Pattern -> D.Pattern
cPattern p = case p of
    A.PTuplePat p1 p2          -> D.PConstr "(,)" $ Prelude.map cPattern [p1,p2]
    A.PTruplePat p1 p2 p3      -> D.PConstr "(,,)" $ Prelude.map cPattern [p1,p2,p3]
    A.PListPat lp              -> cLPat lp
    A.PWild                    -> D.PWild
    A.PId (A.Id name)          -> D.PVar name
    A.PLit l                   -> case l of
        A.LitString str -> strToPat str
        _               -> D.PLit (cLit l)
    A.PConstrEmp (TypeId name) -> D.PConstr name []
    A.PCons p1 p2              -> D.PConstr "Cons" [(cPattern p1), (cPattern p2)]
    A.PDataConstr ts p ps      -> D.PConstr (typeIdentName ts) (map cPattern (p:ps))
    -- (A.PConsConstr (TypeId name) p1 ps p2) -> D.PConstr "Cons" [(D.PConstr name (map cPattern (p1:ps))), (cPattern p2)]
    A.PEmpty                   -> D.PConstr "Nil" []

-- converts a string to a pattern in core syntax
strToPat :: String -> D.Pattern
strToPat []     = D.PConstr "Nil" []
strToPat (c:cs) = D.PConstr "Cons" [D.PLit (D.CLit c), strToPat cs]

-- Converts a list pattern
cLPat :: A.ListPat -> D.Pattern
cLPat (A.PList2 p lp) = D.PConstr "Cons" [(cPattern p), (cLPat lp)]
cLPat (A.PList1 p )    = D.PConstr "Cons" [(cPattern p), (D.PConstr "Nil" [])]


-- Converts a literal
cLit :: A.Literal -> D.Lit
cLit (A.LitInt x)      = D.ILit $ fromInteger x
cLit (A.LitDouble x)   = D.DLit x
cLit (A.LitChar x)     = D.CLit x


-- Converts a constructor
cConst :: A.Cons -> D.Exp
cConst (A.DConst1 (A.TypeId cid) cid' ids) = D.EConstr cid
cConst (A.DConst2 (A.TypeId cid))         = D.EConstr cid

-- Converts a string to an expression in core syntax
strToExp :: String -> D.Exp
strToExp []      = D.EConstr "Nil"
strToExp (c:cs) = D.EApp (D.EApp (D.EConstr "Cons") (D.ELit (D.CLit c))) (strToExp cs)

-- Converts an expression
cExp :: A.Exp -> D.Exp
cExp (A.EVar (A.Id name))   = (D.EVar name)
cExp (A.ETuple t)           = cTuple t
cExp (A.ELiteral lit)       = case lit of
    A.LitString str -> strToExp str
    _               -> (D.ELit $ cLit lit)
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
cExp (A.ESeq e1 e2)         = (D.EBinOp D.Then (cExp e1) (cExp e2))
cExp (A.ECase e cs)         = (D.ECase (cExp e) (cCase cs))
cExp (A.EIf e1 e2 e3)       = (D.ECase (cExp e1) [((D.PConstr "True" []), (cExp e2)),
                                                  ((D.PConstr "False" []), (cExp e3))])
cExp (A.EAbs (A.Id n) ns e) = (D.ELam n (cList' ns e))
    where cList' ((A.Id n):ns) e = (D.ELam n (cList' ns e))
          cList' []            e = cExp e


-- Converts a list of expressions
cList :: [A.Exp] -> D.Exp
cList [] = D.EConstr "Nil"
cList ls = case (head ls) of
    ELiteral (LitString str) -> cStrLitList ls
    ELiteral _               -> cLitList ls
    ENeg     _               -> cLitList ls
    ETuple   _               -> cTupleList ls
    EConst   _               -> cConstList ls
    EList    _               -> cListList ls
    EVar     (Id a)          -> D.EApp ((D.EApp (D.EConstr "Cons") (D.EVar a))) (cList $ tail ls)
    _                        -> error $ "\nError in cList, unable to parse expression: " ++ show (head ls)

cStrLitList :: [A.Exp] -> D.Exp
cStrLitList []     = D.EConstr "Nil"
cStrLitList ((A.ELiteral (A.LitString l)):ls) =
    D.EApp ((D.EApp (D.EConstr "Cons") (strToExp l))) (cStrLitList ls)

-- Converts a list of lists
cListList :: [A.Exp] -> D.Exp
cListList []     = D.EConstr "Nil"
cListList (l:ls) = D.EApp ((D.EApp (D.EConstr "Cons") (cExp l))) (cListList ls)


-- Converts a list of tuples
cTupleList :: [A.Exp] -> D.Exp
cTupleList []     = D.EConstr "Nil"
cTupleList ((A.ETuple t):ts) = D.EApp ((D.EApp (D.EConstr "Cons") (cTuple t))) (cTupleList ts)


-- Converts a list of constructors
cConstList :: [A.Exp] -> D.Exp
cConstList []              = D.EConstr "Nil"
cConstList ((A.EConst c):cs) = D.EApp ((D.EApp (D.EConstr "Cons") (cConst c))) (cConstList cs)


-- Converts a list of literals
cLitList :: [A.Exp] -> D.Exp
cLitList []     = D.EConstr "Nil"
cLitList (l:ls) = let l' = case l of
                            ENeg (ELiteral (LitInt l))    -> LitInt    $ -1*l
                            ENeg (ELiteral (LitDouble l)) -> LitDouble $ -1*l
                            ELiteral l                    -> l
                    in D.EApp ((D.EApp (D.EConstr "Cons") (D.ELit $ cLit l'))) (cLitList ls)


-- Converts a case expression
cCase :: A.Cases -> [(D.Pattern, D.Exp)]
cCase (A.ECases3 p e)    = [((cPattern p),(cExp e))]
cCase (A.ECases1 p e cs) = cCase (A.ECases2 p e cs)
cCase (A.ECases2 p e cs) = ((cPattern p),(cExp e)):(cCase cs)

-- Converts a tuple
cTuple :: A.Tuple -> D.Exp
cTuple (A.Tuple1 e1 e2)    = D.EApp (D.EApp (D.EVar "(,)") (cExp e1)) (cExp e2)
cTuple (A.Tuple2 e1 e2 e3) = D.EApp (D.EApp (D.EApp (D.EVar "(,,)") (cExp e1)) (cExp e2)) (cExp e3)

{--cGuard :: A.Guards -> A.Exp
cGuard (A.DGuards1 e1 e2 gs) = D.ECase (cExp e2) [((D.PConstr "True" []), (cExp e1)),
                                                  ((D.PConstr "False" []), (cGuard gs))]
cGuard (A.DGuards2 e1 e2 gs) = D.ECase (cExp e2) [((D.PConstr "True" []), (cExp e1)),
                                                  ((D.PConstr "False" []), (cGuard gs))]
cGuard (A.DExpGuard e)       = (cExp e)--}
