-- transpiles lollipop data structures into
-- corresponding Haskell data structures

import Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.Syntax as H

import AST.DataTypes
import qualified AST.DataTypes as L


tExp :: L.Exp -> H.Exp
tExp (L.EApp e1 e2)      = undefined
tExp (L.EVar v)          = Var $ UnQual $ Ident v
tExp (L.ELit l)          = Lit $ tLit l
tExp (L.EUnOp op e)      = undefined
tExp (L.EBinOp op e1 e2) = undefined
tExp (L.ELam v e)        = undefined
tExp (L.EConstr cid)     = undefined
tExp (L.ECase e pes)     = undefined
tExp (L.ELetIn v e1 e2)  = undefined
tExp (L.EListComp e1 vvs e2) = undefined

tLit :: L.Lit -> H.Literal
tLit (L.ILit x) = Int $ toInteger x
tLit (L.DLit x) = PrimDouble $ toRational x
tLit (L.CLit x) = Char x
tLit (L.SLit x) = String x
