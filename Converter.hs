
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

cDeclaration :: A.Declaration -> D.Declaration
cDeclaration (A.DFunc (A.Id name) _ defs) = (D.DFunc name vars expr)
    where expr = convertDefs defs
          vars = getArgs defs

convertDefs :: [A.Def] -> D.Exp
convertDefs = undefined

getArgs :: [A.Def] -> D.Vars
getArgs = undefined

cLit :: A.Literal -> D.Lit
cLit (A.LitInt x)      = D.ILit $ fromInteger x
cLit (A.LitDouble x)   = D.DLit x
cLit (A.LitChar x)     = D.CLit x
cLit (A.LitString x)   = D.SLit x

cExp :: A.Exp -> D.Exp
cExp (A.EVar (Id name))   = (D.EVar name)
cExp (A.ELiteral lit)     = (D.ELit $ cLit lit)
-- cExp (A.ELet vID)       = (D.ELetIn )
cExp (A.EApp e1 e2)       = (D.EApp (cExp e1) (cExp e2))
cExp (A.EAdd e1 e2)       = (D.EAdd (cExp e1) (cExp e2))
cExp (A.EAbs (Id name) e) = (D.ELam name (cExp e))

cGuard :: A.Guards -> D.Exp
cGuard (A.DGuards1 e1 e2 gs) = D.EGuard (getGuards gs)
    where getGuards (A.DGuards1 e1' e2' gs') = (((cExp e1'), (cExp e2')):(getGuards gs'))
          getGuards (A.DGuards2 e1' e2' gs') = (((cExp e1'), (cExp e2')):(getGuards gs'))
          getGuards (A.DExpGuard e)          = (((D.EVar "True"),(cExp e)):[])
