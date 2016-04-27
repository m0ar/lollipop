module TypeTest where
import TI
import AST.DataTypes
import Data.Map

testTI = fst $ runTI $ do
    let t = TFun (TVar "b") (TApp (TConstr "[]") (TVar "b")) 
    unify (TFun (TConstr "Int") (TVar "a")) t
    debugTI

testExp :: Exp -> IO()
testExp e = putStrLn $ testExpToString e
    where
        testExpToString :: Exp -> String
        testExpToString e = case runTI (infer (TypeEnv empty) e) of
                (Left error,_) -> show e ++ "\n-- ERROR: " ++ error ++ "\n"
                (Right t,_)    -> show e ++ " :: " ++ show t ++ "\n"


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

list0 = EApp
            (EConstr "Cons")
            eOne

-- Cons 5 Nil -> [5]
list1 = EApp
            (EApp
                (EConstr "Cons")
                eFive)
            (EConstr "Nil")

-- Cons 5 (Cons 2 Nil) -> [5,2]
list2 = EApp
            (EApp
                (EConstr "Cons")
                eFive)
            (EApp
                (EApp
                    (EConstr "Cons")
                    eTwo)
                (EConstr "Nil"))

-- Cons 5 (Cons 2 (Cons 3 Nil)) -> [5,2,3]
list3 = EApp
            (EApp
                (EConstr "Cons")
                eFive)
            (EApp
                (EApp
                    (EConstr "Cons")
                    eTwo)
                (EApp
                    (EApp
                        (EConstr "Cons")
                        eThree)
                    (EConstr "Nil")))

-- Cons 5 (Cons 2 (Cons 3 (Cons 1))) -> [5,2,3,1]
list4 = EApp
            (EApp
                (EConstr "Cons")
                eFive)
            (EApp
                (EApp
                    (EConstr "Cons")
                    (ELit (CLit 'k')))
                (EApp
                    (EApp
                        (EConstr "Cons")
                        eThree)
                    (EApp
                        (EApp
                            (EConstr "Cons")
                            eOne)
                        (EConstr "Nil"))))

-- test expressions
te1  = ELit $ ILit 3

te2  = ELetIn "id" (ELam "x" (EVar "x"))
       (EVar "id")

te3  = ELetIn "id" (ELam "x" (EVar "x"))
       (EApp (EVar "id") (EVar "id"))

te4  = ELetIn "id" (ELam "x" (ELetIn "y" (EVar "x") (EVar "y")))
       (EApp (EVar "id") (EVar "id"))

te5  = ELetIn "id" (ELam "x" (ELetIn "y" (EVar "x") (EVar "y")))
       (EApp (EApp (EVar "id") (EVar "id")) (ELit (ILit 2)))

te6  = ELetIn "id" (ELam "x" (EApp (EVar "x") (EVar "x")))
       (EVar "id")

-- should Succeed
te7  = ECase (ELit(ILit 2)) [(PLit(ILit 2), ELit(CLit 'i')) ,
                             (PLit(ILit 3), ELit(CLit 'u'))
                            ]

-- should Fail, different output types
te8  = ECase (ELit(ILit 2)) [(PLit(ILit 2), ELit(CLit 'i')) ,
                             (PLit(ILit 6), ELit(ILit 9))
                            ]

-- should Fail, different input and matching types
te9  = ECase (ELit(CLit '2')) [(PLit(ILit 2), ELit(CLit 'i')) ,
                               (PLit(ILit 3), ELit(CLit 'u'))
                              ]

te10 = EConstr "True"

te11 = EUnOp Not $ EConstr "False"

--te12 = EApp (EVar "sum") list1
--           where p1    = PConstr "Cons" ["x", "xs2"]
--                 e1    = EBinOp Add (EVar "x")
--                         (EApp (EVar "sum") (EVar "xs2"))
--                 p2    = PConstr "Nil" []
--                 ecase = ECase (EVar "xs") [(p1, e1), (p2, eZero)]
--                 dSum  = DFunc "sum" ["xs"] ecase

main = do
        putStrLn "\n --- TESTING EXPRESSIONS --- \n\n"
        mapM_ testExp allTests
    where
        allTests = [te1,te2,te3,te4,te5,te6,te7,te8,te9,te10,te11]
