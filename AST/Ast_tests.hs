module Ast_tests where

import Interpreter
import Environment
import DataTypes
import Test.QuickCheck

main = do
    t1 <- testHello
    t2 <- testLetIn
    t3 <- testLazyLetIn
    t4 <- testEConstr
    t5 <- testCase
    t6 <- testSumList list4
    t7 <- testSumList2
    t8 <- testLam
    t9 <- testFuncs
    t10 <- testFuncs2
    t11 <- testBinOps
    t12 <- testLazyFuncs

    return (t1
           ,t2
           ,t3
           ,t4
           ,t5
           ,t6
           ,t7
           ,t8
           ,t9
           ,t10
           ,t11
           ,t12
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

-- main = print "HejsaN"
testHello = interpret helloMain -- hello world
    where
        helloMain = [(DFunc "main" [] (EApp
                                        (EVar "print")
                                        (ELit (SLit "HejsaN"))))]

testReadLine = interpret readLine
    where
        readLine = [(DFunc "main" [] (EVar "readLine"))]

testBind = interpret bind
    where
        bind = [(DFunc "main" [] bind')]
        bind' = EApp (EApp (EVar "bind") (EVar "readLine")) (EVar "print")

-- main = let x = 5 + 9 in x + 3
testLetIn = interpret letInMain
    where
        let' = ELetIn "x" (EBinOp Add eFive eNine) (EBinOp Add (EVar "x") eThree)
        letInMain = [DFunc "main" [] let']


-- main = let x = x+1 in 5
testLazyLetIn = interpret lazyLetInMain
    where
        let' = ELetIn "x" (EBinOp Add (EVar "x") eOne) eFive
        lazyLetInMain = [DFunc "main" [] let']


-- main = Cons 5 Nil
testEConstr = interpret [econMain,dCon,dNil]
    where
        econMain = DFunc "main" [] (EApp (EApp (EConstr "Cons") (eFive)) (EConstr "Nil"))


-- main = case (Cons 2 Nil) of
--      Cons x xs -> x + 0
--      Nil       -> 0
testCase = interpret caseMain
    where elist    = list1
          -- elist = EConstr "Nil" []
          p1       = (Constr "Cons" ["x", "xs"] (EBinOp Add (EVar "x") eZero))
          p2       = (Constr "Nil" [] eZero)
          ecase    = ECase elist [p1, p2]
          caseMain = [(DFunc "main" [] ecase), dCon, dNil]

{-
sum xs = case xs of
    Cons x xs2  ->  x + sum xs2
    Nil         -> 0
-}
testSumList :: Exp -> IO ()
testSumList l = interpret [dMain, dSum, dCon, dNil]
    where dMain = DFunc "main" [] (EApp (EVar "sum") l)
          p1    = (Constr "Cons" ["x", "xs2"] (EBinOp Add (EVar "x")
                  (EApp (EVar "sum") (EVar "xs2"))))
          p2    = (Constr "Nil" [] eZero)
          ecase = ECase (EVar "xs") [p1, p2]
          dSum  = DFunc "sum" ["xs"] ecase

{-
main = sum list1
sum xs = case xs of
    Nil        -> 0
    Cons x xs' -> x + sum xs
-}
testSumList2 = interpret [dMain, dSum, dCon, dNil]
    where
        dMain = DFunc "main" [] (EApp (EVar "sum") list1)
        dSum  = DFunc "sum" ["xs"] (ECase (EVar "xs") [p1, p2])
            where
                p1 = (Constr "Nil" [] eZero)
                p2 = (Constr "Cons" ["x", "xs'"] (EBinOp Add (EVar "x")
                     (EApp (EVar "sum") (EVar "xs'"))))


-- main = (\x -> x + 4) ((\x -> x + 4) 6)   -- should return 14
testLam = interpret lamMain -- lambda-calculus addition with application
    where lam = EApp (ELam "x" (EBinOp Add (EVar "x") eFour))
                    (EApp (ELam "x" (EBinOp Add (EVar "x")
                    eFour)) eSix)
          lamMain = [(DFunc "main" [] lam)]


-- main = add 5 2
-- add x y = x + y
testFuncs = interpret funcMain
    where funcMain = [
                        (DFunc "main" [] (EApp (EApp (EVar "add")
                        eFive) eTwo)),
                        (DFunc "add" ["x","y"] (EBinOp Add (EVar "x") (EVar "y")))
                     ]

-- main = add 3
-- add x = x + 2
testFuncs2 = interpret funcMain
    where funcMain = [
                        (DFunc "main" [] (EApp (EVar "add") eThree)),
                        (DFunc "add" ["x"] (EBinOp Add (EVar "x") eTwo))
                     ]

testBinOps = interpret $ [DFunc "main" []
    (EApp (EApp (EVar "add") eTwo)
          (EApp (EApp (EVar "mul") eThree) eFour))]

-- main = first 5 (infty 0)
-- first x y = x
-- infty x = 1 + infty x
testLazyFuncs = interpret [funcMain, funcFirst, funcInfty] where
    funcMain = DFunc "main" [] (EApp (EApp (EVar "first") eFive) (EApp (EVar "infty") eZero))
    funcFirst = DFunc "first" ["x", "y"] (EVar "x")
    funcInfty = DFunc "infty" ["x"] (EBinOp Add eOne (EApp (EVar "infty") (EVar "x")))

{-
test2 = interpret ds >>= putStrLn . take 1000 . show where
    cons = "Cons"
    ds   = [main,go,map]
    main = DFunc "main" [] body where
        body = EApp (EVar "go") eOne
    go   = DFunc "go" ["x"] body where
        x    = EVar "x"
        body = EConstr cons [x, EVar "go" `EApp` (x `EBinOp Add` x)]
    map  = DFunc "map" [f,"xs0"] body where
        (f,x,xs) = ("f","x","xs")
        body = ECase (EVar "xs0")
            [(cons,[x,xs],EConstr cons [EVar f `EApp` EVar x,
              EVar "map" `EApp` EVar f `EApp` EVar xs])
            ,("Nil",[],EConstr "Nil" [])
            ]
-}
