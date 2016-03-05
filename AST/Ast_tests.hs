module Ast_tests where

import Frame
import Environment
import DataTypes
import Test.QuickCheck
{-
main = do
    a <- testIf
    b <- testHello
    c <- testLam
    d <- testFuncs2
    e <- testFuncs
    f <- test2
    g <- test1
    h <- testSumList2
    i <- testSumList
    j <- testCase
    k <- testCon
    l <- testGuard
    m <- testLetIn
    return (a
           ,b
           ,c
           ,d
           ,e
           ,f
           ,g
           ,h
           ,i
           ,j
           ,k
           ,l
           ,m)
-}
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

{-
eList1 = EConstr "Cons" [eTwo, EConstr "Cons" [eOne, EConstr "Cons" [eNine, (EConstr "Nil" [])]]]
eList2 = EConstr "Cons" [eTwo, EConstr "Cons" [eThree, EConstr "Cons" [eNine, (EConstr "Nil" [])]]]
-}
-- Test Where
testWhere = interpret whereMain
    where
        where' = EWhere "x" (EAdd (EVar "x") eThree) (EAdd eFive eNine)
        whereMain = [DFunc "main" [] where']

-- Test Let
testLetIn = interpret letInMain
    where
        let' = ELetIn "x" (EAdd eFive eNine) (EAdd (EVar "x") eThree)
        letInMain = [DFunc "main" [] let']
{-
-- should return "second guard reached"
testGuard = interpret guardMain
    where ts = [((ELit (BLit False)), (EPrint (ELit (SLit "first case reached")))),
                ((ELit (BLit True)), (EPrint (ELit (SLit "second case reached"))))]
          guard = EGuard ts (EPrint (ELit (SLit "otherwise case reached")))
          guardMain = [(DFunc "main" [] guard)]
-}

-- test ECase
-- main = case (Cons 2 Nil) of
--      Cons x xs -> x + 0
--      Nil       -> 0
testCase = interpret caseMain
    where elist    = list1
          -- elist = EConstr "Nil" []
          p1       = (Constr "Cons" ["x", "xs"] (EAdd (EVar "x") eZero))
          p2       = (Constr "Nil" [] eZero)
          ecase    = ECase elist [p1, p2]
          caseMain = [(DFunc "main" [] ecase), dCon, dNil]

{-
sum xs = case xs of
    Cons x xs2  ->  x + sum xs2
    Nil         -> 0
-}
testSumList :: Exp -> IO Value
testSumList l = interpret [dMain, dSum, dCon, dNil]
    where dMain = DFunc "main" [] (EApp (EVar "sum") l)
          p1    = (Constr "Cons" ["x", "xs2"] (EAdd (EVar "x")
                  (EApp (EVar "sum") (EVar "xs2"))))
          p2    = (Constr "Nil" [] eZero)
          ecase = ECase (EVar "xs") [p1, p2]
          dSum  = DFunc "sum" ["xs"] ecase

-- Another sumList test
testSumList2 = interpret [dMain, dSum, dCon, dNil]
    where
        dMain = DFunc "main" [] (EApp (EVar "sum") list1)
        dSum  = DFunc "sum" ["xs"] (ECase (EVar "xs") [p1, p2])
            where
                p1 = (Constr "Nil" [] eZero)
                p2 = (Constr "Cons" ["x", "xs'"] (EAdd (EVar "x")
                     (EApp (EVar "sum") (EVar "xs'"))))
{-
test1 = interpret ds >>= putStrLn . take 1000 . show where
  ds   = [main]
  main = DFunc "main" [] body
  body = EConstr "Cons" [(ELit (ILit 1)), EVar "main"]

test2 = interpret ds >>= putStrLn . take 1000 . show where
    cons = "Cons"
    ds   = [main,go,map]
    main = DFunc "main" [] body where
        body = EApp (EVar "go") (ELit (ILit 1))
    go   = DFunc "go" ["x"] body where
        x    = EVar "x"
        body = EConstr cons [x, EVar "go" `EApp` (x `EAdd` x)]
    map  = DFunc "map" [f,"xs0"] body where
        (f,x,xs) = ("f","x","xs")
        body = ECase (EVar "xs0")
            [(cons,[x,xs],EConstr cons [EVar f `EApp` EVar x,
              EVar "map" `EApp` EVar f `EApp` EVar xs])
            ,("Nil",[],EConstr "Nil" [])
            ]
-}

-- main-test functions
testFuncs = interpret funcMain
    where funcMain = [
                        (DFunc "main" [] (EApp (EApp (EVar "add")
                        (ELit (ILit 5))) (ELit (ILit 2)))),
                        (DFunc "add" ["x","y"] (EAdd (EVar "x") (EVar "y")))
                    ]

-- main-test functions
testFuncs2 = interpret funcMain
    where funcMain = [  (DFunc "main" [] (EApp (EVar "add") (ELit (ILit 3)))),
                        (DFunc "add" ["x"] (EAdd (EVar "x") (ELit (ILit 2))))
                    ]
-- 14
testLam = interpret lamMain -- lambda-calculus addition with application
    where lam = EApp (ELam "x" (EAdd (EVar "x") (ELit (ILit 4))))
                    (EApp (ELam "x" (EAdd (EVar "x")
                    (ELit (ILit 4)))) (ELit (ILit 6)))
          lamMain = [(DFunc "main" [] lam)]

-- hi
testHello = interpret helloMain -- hello world
    where
        hello = EPrint (ELit (SLit "hi"))
        helloMain = [(DFunc "main" [] hello)]

-- noes
testIf = interpret ifTestMain -- simple if-statement with printout
    where
        ifTest = EIf (ELit (BLit False))
            ((EPrint (ELit (SLit "hi")))) ((EPrint (ELit (SLit "noes"))))
        ifTestMain = [(DFunc "main" [] ifTest)]


testECon2 = interpret [econMain,dcon,dnil]
    where
        econMain = DFunc "main" [] (EApp (EApp (EConstr "cons") (eFive)) (EConstr "nil"))
        dcon = DConstr "cons" (VFun (\v1 -> VFun (\v2 -> VConstr "cons" [v1,v2])))
        dnil = DConstr "nil" (VConstr "nil" [])
