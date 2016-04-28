function putStrLn : Int ;
putStrLn x := print x ;

function testLam : Int -> Int ;
testLam := (\y -> head y) (tail [1,2,3]) ;

function sumFst : [(Int,Int)] -> Int ;
sumFst (x:[]) := fst x ;
sumFst (x:xs) := (fst x) + (sumFst xs) ;

function sumFstList : [[Int]] -> Int ;
sumFstList (x:[]) := head x ;
sumFstList (x:xs) := (head x) + (sumFstList xs) ;

function fst : [(Int,Int)] -> Int ;
fst (x,y) := x ;

function sum : [Int] -> Int ;
sum (x:[]) := x ;
sum (x:xs) := x + sum xs ;

function head : [Int] -> Int ;
head (x:[]) := x ;
head (x:xs) := x ;

function snd : [Int] -> Int ;
snd (x:xs) := head xs ;

function trd : [Int] -> Int ;
trd (x:xs) := snd xs ;

function tail : [Int] -> Int ;
tail (x:[]) := x ;
tail (x:xs) := xs ;

function last : [Int] -> Int ;
last y := case y of
    (x:[]) -> x
    (x:xs) -> last xs

function lst : [Int] ;
lst := [True,False,True] ;

function tpl : (Int, Int) ;
tpl := (1, 2) ;

function bl : Bool ;
bl := True ;

function tpl2 : (Int, Int) -> Int ;
tpl2 (1, 2) := 12 ;
tpl2 (x, y) := x-y ;
tpl2 (3, 4) := 34 ;

function tpl3 : Int -> (Int, Int, Int) -> Int ;
tpl3 x (3, y, 3) := 6-3 ;

function one : Int ;
one := 1 ;

function pat : Int -> Int -> Int -> Int ;
pat 0 x 2 := 5 ;
pat 0 1 _ := 12 ;
pat _ _ _ := 14 ;

function patR : Int -> Int -> Int -> Int ;
patR x 2 5 := x + 1 ;
patR y 2 2 := 2 ;

function grd : Int ;
grd := 5 when True
    := 2 ;

function useDbl : Int -> Int ;
useDbl x := 4 + dbl x ;

function dbl : Int -> Int ;
dbl x := case x of
    1 -> 2
    x -> 2 * x

function ca : Int -> Int -> Int -> Int ;
ca 3 x 2 := case x of
    2 -> 2 + x
    _ -> 10
ca _ 0 0 := 0

function onePat : Int -> Int ;
pat 2 := 11 ;
pat _ := 0 ;

--example from learnyouahaskell
function zipWith : (a -> b -> c) -> [a] -> [b] -> [c] ;
zipWith _ []     _      := [] ;
zipWith _ _      []     := [] ;
zipWith f (x:xs) (y:ys) := f x y : zipWith' f xs ys ;

--type A := [(String,Int)] ;
--type B := ([String] -> Int) -> Bool ;
datatype Maybe a := Just a b | Nothing

datatype Gender := Man a b | Woman Int | Frog

datatype Cake a := Sweet a | Lie

datatype Value :=
        VIO (IO Value) -- void IO
        | VString String -- TODO Remove
        | VLit Lit
        | VConstr ConstrID [Value] -- list of values to be used as parameters
        | VFun (Value -> Value)
