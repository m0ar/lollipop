module Ast_tests where

import Frame
import Test.QuickCheck

main = do
    a <- testLam
    b <- testHello
    c <- testIf
    d <- testSumList
    return (a
           ,b
           ,c
           ,d)

-- should be: (10,hi,noes,235)

--testCase = interpret caseMain
    --where caseMain = ECase (EVar "x") ps
    --      ps = [("x", [], ()),()]

eTwo = ELit (ILit 2)

-- test ECon
testCon = interpret conMain
    where con = ECon "Cons" [eTwo, (ECon "Nil" [])]
          conMain = [(DFunc "main" [] con)]
    
-- main-test functions
testFuncs = interpret funcMain
    where funcMain = [
                        (DFunc "main" [] (EApp (EApp (EVar "add") (ELit (ILit 5))) (ELit (ILit 2)))),
                        (DFunc "add" ["","x","y"] (EAdd (EVar "x") (EVar "y")))
                    ]

-- main-test functions
testFuncs1 = interpret funcMain
    where funcMain = [  (DFunc "main" [] (EApp (EVar "add") (ELit (ILit 3)))),
                        (DFunc "add" ["x"] (EAdd (EVar "x") (ELit (ILit 2))))
                    ]

-- 14
testLam = interpret lamMain -- lambda-calculus addition with application
    where lam = EApp (ELam "x" (EAdd (EVar "x") (ELit (ILit 4)))) (EApp (ELam "x" (EAdd (EVar "x") (ELit (ILit 4)))) (ELit (ILit 6)))
          lamMain = [(DFunc "main" [] lam)]

-- hi
testHello = interpret helloMain -- hello world
    where
        hello = EPrint (ELit (SLit "hi"))
        helloMain = [(DFunc "main" [] hello)]

-- noes
testIf = interpret ifTestMain -- simple if-statement with printout
    where
        ifTest = EIf (ELit (BLit False)) ((EPrint (ELit (SLit "hi")))) ((EPrint (ELit (SLit "noes"))))
        ifTestMain = [(DFunc "main" [] ifTest)]

--235
testSumList = interpret sumListMain -- simple list
    where
        sumList = Cons (ILit 28) (Cons (ILit 198) (Cons (ILit 2) (Cons (ILit 3) Nil)))
        sumListMain = [(DFunc "main" [] (EAdd (ELit (ILit 4)) (EList sumList)))]
