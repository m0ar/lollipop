module Ast_tests where

import Interpreter
import Environment
import DataTypes
import Test.QuickCheck

main = do
    t1 <- testHello
    t2 <- testLetIn
    t3 <- testLazyLetIn
    -- t4 <- testEConstr
    --t5 <- testCase
    --t6 <- testSumList list4
    --t7 <- testSumList2
    t8 <- testLam
    t9 <- testFuncs
    t10 <- testFuncs2
    t11 <- testBinOps
    t12 <- testLazyFuncs
    t13 <- testThen

    return (t1
           ,t2
           ,t3
           -- ,t4
           -- ,t5
           --,t6
           --,t7
           ,t8
           ,t9
           ,t10
           ,t11
           ,t12
           ,t13
            )


--------------------------------------------------------------------------------
-- things to use in tests
--------------------------------------------------------------------------------
dCon = DConstr "Cons" (VFun (\v1 -> VFun (\v2 -> VConstr "Cons" [v1,v2])))
dNil = DConstr "Nil" (VConstr "Nil" [])

eZero   = ELit (ILit 0)
eOne    = ELit (ILit 1)
eTwo    = ELit (ILit 2)
eThree  = ELit (ILit 3)
eFour   = ELit (ILit 4)
eFive   = ELit (ILit 5)
eSix    = ELit (ILit 6)
eSeven  = ELit (ILit 7)
eEight  = ELit (ILit 8)
eNine   = ELit (ILit 9)
x       = EVar "x"
y       = EVar "x"
z       = EVar "x"

-- Cons 5 Nil -> [5]
list1 = (EApp (EApp (EConstr "Cons") (eFive)) (EConstr "Nil"))

-- Cons 5 (Cons 2 Nil) -> [5,2]
list2 = (EApp
            (EApp (EConstr "Cons") (eFive))
            (EApp (EApp (EConstr "Cons") (eTwo)) (EConstr "Nil"))
        )

-- Cons 5 (Cons 2 (Cons 3 Nil)) -> [5,2,3]
list3 = (EApp
            (EApp (EConstr "Cons") (eFive))
            (EApp (EApp (EConstr "Cons") (eTwo))
            (EApp (EApp (EConstr "Cons") (eThree)) (EConstr "Nil")))
        )

-- Cons 5 (Cons 2 (Cons 3 (Cons 1))) -> [5,2,3,1]
list4 = (EApp
            (EApp (EConstr "Cons") (eFive))
            (EApp (EApp (EConstr "Cons") (eTwo))
            (EApp (EApp (EConstr "Cons") (eThree))
            (EApp (EApp (EConstr "Cons") (eOne)) (EConstr "Nil"))))
        )

--------------------------------------------------------------------------------
-- tests
--------------------------------------------------------------------------------
{-
main = print "HejsaN"
-}
testHello = interpret helloMain -- hello world
    where
        helloMain = [(DFunc "main" [] (EApp
                                        (EVar "print")
                                        (ELit (SLit "HejsaN"))))]
{-
Testing of user input.
-}
testReadLine = interpret readLine
    where
        readLine = [(DFunc "main" [] (EVar "readLine"))]

{-
Testing of cLiting a user input. The input funcs result is passed to the print function.
-}
testBind = interpret bind
    where
        bind = [(DFunc "main" [] bind')]
        bind' = EApp (EApp (EVar "bind") (EVar "readLine")) (EVar "print")

testThen = interpret thenF
    where
        thenF = [(DFunc "main" [] thenF')]
        thenF' = EApp (EApp (EVar "then") (EApp (EVar "print") (ELit (SLit "Hello")))) (EApp (EVar "print") (ELit (SLit "Jonas")))


{-
main = let x = 5 + 9 in x + 3
-}
testLetIn = interpret letInMain
    where
        let' = ELetIn "x" (EBinOp Add eFive eNine) (EBinOp Add (EVar "x") eThree)
        letInMain = [DFunc "main" [] let']

{-
main = let x = x+1 in 5
-}
testLazyLetIn = interpret lazyLetInMain
    where
        let' = ELetIn "x" (EBinOp Add (EVar "x") eOne) eFive
        lazyLetInMain = [DFunc "main" [] let']

{-
main = Cons 5 Nil
-}
testEConstr = interpret [econMain,dCon,dNil]
    where
        econMain = DFunc "main" [] (EApp (EApp (EConstr "Cons") (eFive)) (EConstr "Nil"))

{-- testCase = interpret caseMain
    where elist    = list1
          -- elist = EConstr "Nil" []
          p1       = PConstr "Cons" ["x", "xs"]
          e1       = EBinOp Add (EVar "x") eZero
          p2       = PConstr "Nil" []
          ecase    = ECase elist [(p1,e1), (p2,eZero)]
          caseMain = [(DFunc "main" [] ecase), dCon, dNil]

testCase1 = interpret caseMain
    where caseMain = [(DFunc "main" ["x"] ecase)]
          ecase    = ECase (EVar "x") [(p,e)]
          p        = PConstr "main" []
          e        = ELit (ILit 2) --}

{-- testSumList :: Exp -> IO ()
testSumList l = interpret [dMain, dSum, dCon, dNil]
    where dMain = DFunc "main" [] (EApp (EVar "sum") l)
          p1    = PConstr "Cons" ["x", "xs2"]
          e1    = EBinOp Add (EVar "x")
                  (EApp (EVar "sum") (EVar "xs2"))
          p2    = PConstr "Nil" []
          ecase = ECase (EVar "xs") [(p1, e1), (p2, eZero)]
          dSum  = DFunc "sum" ["xs"] ecase --}

testPattern1 = interpret [(DFunc "main" [] lam)]
    where p1    = PLit (ILit 5)
          e1    = EApp (EVar "print") (ELit (SLit "First case"))
          p2    = PWild
          e2    = EApp (EVar "print") (ELit (SLit "Second case"))
          lam   = EApp (ELam "x" (ECase x [(p1,e1),(p2,e2)])) eZero


{-- testSumList2 = interpret [dMain, dSum, dCon, dNil]
    where
        dMain = DFunc "main" [] (EApp (EVar "sum") list1)
        dSum  = DFunc "sum" ["xs"] (ECase (EVar "xs") [(p1, eZero), (p2, e2)])
            where
                p1 = PConstr "Nil" []
                p2 = PConstr "Cons" ["x", "xs'"]
                e2 = EBinOp Add (EVar "x") (EApp (EVar "sum") (EVar "xs'"))
--}
{-
main = (\x -> x + 4) ((\x -> x + 4) 6)   -- should return 14
-}
testLam = interpret lamMain -- lambda-calculus addition with application
    where lam = EApp (ELam "x" (EBinOp Add (EVar "x") eFour))
                    (EApp (ELam "x" (EBinOp Add (EVar "x")
                    eFour)) eSix)
          lamMain = [(DFunc "main" [] lam)]

{-
main = add 5 2
add x y = x + y
-}
testFuncs = interpret funcMain
    where funcMain = [
                        (DFunc "main" [] (EApp (EApp (EVar "adds")
                        eFive) eTwo)),
                        (DFunc "adds" ["x","y"] (EBinOp Add (EVar "x") (EVar "y")))
                     ]
{-
main = add 3
add x = x + 2
-}
testFuncs2 = interpret funcMain
    where funcMain = [
                        (DFunc "main" [] (EApp (EVar "add") eThree)),
                        (DFunc "add" ["x"] (EBinOp Add (EVar "x") eTwo))
                     ]

 {-
 main = 2 + 3
 -}
testAdd = interpret addMain
    where addMain = [(DFunc "main" [] (EBinOp Add (ELit (ILit 2)) (ELit (ILit 3))))]

{-
main = 2+3*4
-}
testBinOps = interpret $ [DFunc "main" []
    (EApp (EApp (EVar "#add") eTwo)
          (EApp (EApp (EVar "#mul") eThree) eFour))]

{-
main = first 5 (infty 0)
first x y = x
infty x = 1 + infty x
-}
testLazyFuncs = interpret [funcMain, funcFirst, funcInfty] where
    funcMain = DFunc "main" [] (EApp (EApp (EVar "first") eFive) (EApp (EVar "infty") eZero))
    funcFirst = DFunc "first" ["x", "y"] (EVar "x")
    funcInfty = DFunc "infty" ["x"] (EBinOp Add eOne (EApp (EVar "infty") (EVar "x")))
