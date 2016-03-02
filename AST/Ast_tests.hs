module Ast_tests where

import Frame
import Test.QuickCheck

main = do
    a <- testLam
    b <- testHello
    c <- testIf
    return (a
           ,b
           ,c)

-- should be: (10,hi,noes,235)

--testCase = interpret caseMain
    --where caseMain = ECase (EVar "x") ps
    --      ps = [("x", [], ()),()]

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

-- test ECon
-- main = Cons 2 Nil
testCon = interpret conMain
    where con = ECon "Cons" [eTwo, (ECon "Nil" [])]
          conMain = [(DFunc "main" [] con)]

-- test ECase
-- main = case (Cons 2 Nil) of
--      Cons x xs -> x + 0
--      Nil       -> 0
testCase = interpret caseMain
    where elist    = ECon "Cons" [eTwo, (ECon "Nil" [])]
          -- elist = ECon "Nil" []
          p1       = ("Cons", ["x", "xs"], (EAdd (EVar "x") eZero))
          p2       = ("Nil", [], eZero)
          ecase    = ECase elist [p1, p2]
          caseMain = [(DFunc "main" [] ecase)]

{-
main = sum (Cons 2 (Cons 1 (Cons 9 Nil)))

sum xs = case xs of
    Cons x xs2  ->  x + sum xs2
    Nil         -> 0
-}

testSumList = interpret [dMain, dSum]
    where elist = ECon "Cons" [eTwo, ECon "Cons" [eOne, ECon "Cons" [eNine, (ECon "Nil" [])]]]
          dMain = DFunc "main" [] (EApp (EVar "sum") elist)
          p1    = ("Cons", ["x", "xs2"], (EAdd (EVar "x") (EApp (EVar "sum") (EVar "xs2"))))
          p2    = ("Nil", [], eZero)
          ecase = ECase (EVar "xs") [p1, p2]
          dSum  = DFunc "sum" ["xs"] ecase

-- Gets stuck in a loop...
testSumList2 = interpret [dmain]
    where
        dmain = DFunc "sum" ["xs"] (ECase (ECon "Cons" [eTwo, (ECon "Nil" [])]) [p1, p2])
            where
                p1 = ("Nil", [], eZero)
                p2 = ("Cons", ["x", "xs"], (EAdd (EVar "x") (EApp (EVar "sum") (EVar "xs"))))

test1 = interpret ds >>= putStrLn . take 1000 . show where
  ds = [main]
  main = DFunc "main" [] body
  body = ECon "Cons" [(ELit (ILit 1)), EVar "main"]


test2 = interpret ds >>= putStrLn . take 1000 . show where
  cons = "Cons"
  ds = [main,go,map]
  main = DFunc "main" [] body where
    body = EApp (EVar "go") (ELit (ILit 1))

  go = DFunc "go" ["x"] body where
    x = EVar "x"
    body = ECon cons [x, EVar "go" `EApp` (x `EAdd` x)]

  map = DFunc "map" [f,"xs0"] body where
    (f,x,xs) = ("f","x","xs")
    body = ECase (EVar "xs0")
     [(cons,[x,xs],ECon cons [EVar f `EApp` EVar x,EVar "map" `EApp` EVar f `EApp` EVar xs])
     ,("Nil",[],ECon "Nil" [])
     ]
          
    
-- main-test functions
testFuncs = interpret funcMain
    where funcMain = [
                        (DFunc "main" [] (EApp (EApp (EVar "add") (ELit (ILit 5))) (ELit (ILit 2)))),
                        (DFunc "add" ["x","y"] (EAdd (EVar "x") (EVar "y")))
                    ]

-- main-test functions
testFuncs1 = interpret funcMain
    where funcMain = [  (DFunc "main" [] (EApp (EVar "add") (ELit (ILit 3)))),
                        (DFunc "add" ["x"] (EAdd (EVar "x") (ELit (ILit 2))))
                    ]
-- 14
testLam = interpret lamMain -- lambda-calculus addition with application
    where lam = EApp (ELam "x" (EAdd (EVar "x") (ELit (ILit 4))))
                    (EApp (ELam "x" (EAdd (EVar "x") (ELit (ILit 4)))) (ELit (ILit 6)))
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
{-
testSumList = interpret sumListMain -- simple list
    where
        sumList = Cons (ILit 28) (Cons (ILit 198) (Cons (ILit 2) (Cons (ILit 3) Nil)))
        sumListMain = [(DFunc "main" [] (EAdd (ELit (ILit 4)) (EList sumList)))]
-}