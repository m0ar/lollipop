-- transpiles lollipop data structures into
-- corresponding Haskell data structures

import Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.Syntax as H

import AST.DataTypes
import qualified AST.DataTypes as L

-- Transpiles an Exp in lollipop to an Exp in Haskell
tExp :: L.Exp -> H.Exp
tExp (L.EApp e1 e2)      = App (tExp e1) (tExp e2)
tExp (L.EVar v)          = Var $ UnQual $ Ident v
tExp (L.ELit l)          = Lit $ tLit l
tExp (L.EUnOp op e)      = undefined
tExp (L.EBinOp op e1 e2) = undefined
tExp (L.ELam v e)        = undefined
tExp (L.EConstr cid)     = undefined
tExp (L.ECase e pes)     = undefined
tExp (L.ELetIn v e1 e2)  = Let (IPBinds [(tIPBind ("",0,0) v e1)]) (tExp e2)
tExp (L.EListComp e1 vvs e2) = undefined

-- Transpiles a Lit in lollipop to a Literal in Haskell
tLit :: L.Lit -> H.Literal
tLit (L.ILit x) = Int $ toInteger x
tLit (L.DLit x) = PrimDouble $ toRational x
tLit (L.CLit x) = Char x
tLit (L.SLit x) = String x

-- Creates a IPBind (A binding of an implicit parameter) from
-- a soruce (String, Int, Int) of an expression to a variable
tIPBind :: (String, Int, Int) -> L.Var -> L.Exp -> H.IPBind
tIPBind (src, line, column) v e = IPBind (SrcLoc src line column)
                                         (IPDup v)
                                         (tExp e)
