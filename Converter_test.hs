-- runghc -iAST/:grammar/ Converter_test.hs

module Converter_tests where

import Interpreter
import Converter
import qualified DataTypes as D
import AbsGrammar
import qualified AbsGrammar as A

main :: IO ()
main = interpret $ cProgram p1
    where p1 = A.PLast (A.DFunc (A.Id "main") (A.STypeDecl (A.TTypeId (A.STypeIdent (A.TypeId "Int")))) [A.DDef (A.Id "main") [] (A.EAdd (A.ELiteral (A.LitInt 2)) (A.ELiteral (A.LitInt 5)))])

{-- p1:
function main : Int ;
main := 2+5;
--}
