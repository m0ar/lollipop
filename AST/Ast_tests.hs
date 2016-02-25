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


-- 10
testLam = interpret lamMain -- lambda-calculus addition with application
    where lam = EApp (ELam "x" (EAdd (EVar "x") (ELit (ILit 4)))) (ELit (ILit 6))
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
