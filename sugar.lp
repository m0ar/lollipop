function power : Int -> Int -> Int ;
power x 0 := 1 ;
power x n := x * (power x (n-1)) ;

function fac : a -> b ;
fac 0 := 1 ;
fac x := x * (fac (x-1)) ;

function fst : (a,b) -> a ;
fst (x,y) := x ;

function snd : (a,b) -> b ;
snd (x,y) := y ;

function head : [Int] -> Int ;
head []     := Undefined ;
head (x:xs) := x ;

function tail : [Int] -> [Int] ;
tail []     := Undefined ;
tail (x:xs) := xs ;

function sumList : [Int] -> Int ;
sumList []     := 0 ;
sumList (x:[]) := x ;
sumList (x:xs) := x + sumList xs ;

function last : [Int] -> Int ;
last []     := Undefined ;
last (x:[]) := x ;
last (x:xs) := last xs ;
