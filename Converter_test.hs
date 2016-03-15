-- runghc -iAST/:grammar/ Converter_test.hs

module Converter_tests where

import Interpreter
import Converter
import qualified DataTypes as D
import AbsGrammar
import qualified AbsGrammar as A

main :: IO ()
main = interpret $ cProgram p1  -- interpret $ cProgram p1
    where p0 = A.PLast (A.DFunc (A.Id "main") (A.MTypeDecl (A.TTypeId (A.STypeIdent (A.TypeId "Int"))) (A.STypeDecl (A.TTypeId (A.STypeIdent (A.TypeId "Int"))))) [A.DDef (A.Id "main") [A.DArg4 (A.P3 (A.PId (A.Id "x")))] (A.ECase (A.ELiteral (A.LitInt 5)) (A.ECases1 (A.P3 (A.PLit (A.LitInt 5))) (A.ELiteral (A.LitInt 10)) (A.ECases2 (A.P3 (A.PLit (A.LitInt 0))) (A.ELiteral (A.LitInt 0)) A.ECases3)))])
          p1 = A.PLast (A.DFunc (A.Id "main") (A.STypeDecl (A.TTypeId (A.STypeIdent (A.TypeId "Int")))) [A.DDef (A.Id "main") [] (A.ELiteral (A.LitInt 2))])
          p2 = A.PLast (A.DFunc (A.Id "main") (A.STypeDecl (A.TTypeId (A.STypeIdent (A.TypeId "Int")))) [A.DDef (A.Id "main") [] (A.EAdd (A.ELiteral (A.LitInt 2)) (A.ELiteral (A.LitInt 5)))])
          p3 = A.PLast (A.DFunc (A.Id "main")
                                (A.MTypeDecl (A.TTypeId (A.STypeIdent (A.TypeId "Int")))
                                             (A.MTypeDecl (A.TTypeId (A.STypeIdent (A.TypeId "Int")))
                                                          (A.STypeDecl (A.TTypeId (A.STypeIdent (A.TypeId "Int"))))))
                                [A.DDef (A.Id "main")
                                       [A.DArg4 (A.P3 (A.PLit (A.LitInt 0))),
                                        A.DArg4 (A.P3 (A.PLit (A.LitInt 1)))]
                                        (A.EAdd (A.ELiteral (A.LitInt 5))
                                              (A.ELiteral (A.LitInt 9))),
                                A.DDef (A.Id "main")
                                        [A.DArg4 (A.P3 A.Pwild),
                                         A.DArg4 (A.P3 (A.PLit (A.LitInt 2)))]
                                         (A.EAdd (A.ELiteral (A.LitInt 2))
                                                 (A.ELiteral (A.LitInt 3)))])


{-- p1:
function main : Int ;
main := 2+5;
--}
