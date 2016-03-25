function empty : [Int] ;
empty := [] ;

function lst1 : [Bool] ;
lst1 := [True,False,True] ;

function head : [Int] -> Int ;
head [] := 0 ;
head (x:xs) := x ;

function lstPat1 : [Int] -> Int ;
lstPat1 ys := case ys of
    []      -> 0 ;
    (1:xs)  -> 1 ;
    (x:[])  -> x ;
    (_:xs)  -> 3 ; ;
    
function lstPat2 : [Int] -> Int ;
lstPat2 [] := 0 ;
lstPat2 (1:xs) := 1 ;
lstPat2 (x:[]) := x ;
lstPat2 (_:xs) := 3 ;

function sum : [Int] -> Int ;
sum [] := 0 ;
sum (x:xs) := x + sum xs ;

function last : [Int] -> Int ;
last ys := case ys of
    (x:[]) -> x ;
    (x:xs) -> last xs ;;
    

-- Can't load file when these functions are included
----------------------------------------------------
{-

-- list of ELiteral
function lst2 : [Int] ;
lst2 := [1,2,3] ;

-- list of ETuple
function lst3 : [Tuple] ;
lst3 := [(1,2), (3,4)] ;

-}