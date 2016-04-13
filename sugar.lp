-- mathematical functions

function power : Int -> Int -> Int ;
power x 0 := 1 ;
power x n := x * (power x (n-1)) ;

function fac : a -> b ;
fac 0 := 1 ;
fac x := x * (fac (x-1)) ;

function fib : Int -> Int ;
fib 0 := 0 ;
fib 1 := 1 ;
fib n := fib (n-2) +
         fib (n-1) ;

function div : Int -> Int -> Int ;
div _ 0 := Undefined ;
div 0 _ := 0 ;
div x y
    := div -x -y        when y < 0
    := -1 + div (x+y) y when x < 0
    := 1 + div (x-y) y  when x > y
    := 1                when x == y
    := 0 ;

function mod : Int -> Int -> Int ;
mod x y := x - y * div x y ;



-- logical functions

function odd : Int -> Boolean ;
odd x := mod x 2 == 1 ;

function even : Int -> Boolean ;
even x := mod x 2 == 0 ;



-- functions over tuples

function fst : (a,b) -> a ;
fst (x,y) := x ;

function snd : (a,b) -> b ;
snd (x,y) := y ;

function max : (a,a) -> a ;
max (x,y)
    := x when x > y
    := y ;

function min : (a,a) -> a ;
min (x,y)
    := x when x < y
    := y ;



-- functions over lists

function head : [a] -> a ;
head []     := Undefined ;
head (x:xs) := x ;

function tail : [a] -> [a] ;
tail []     := Undefined ;
tail (x:xs) := xs ;

function last : [a] -> a ;
last []     := Undefined ;
last (x:[]) := x ;
last (x:xs) := last xs ;

function take : Int -> [a] -> [a] ;
take n []     := [] ;
take 0 _      := [] ;
take n (x:xs) := x:(take (n-1) xs) ;

function length : [Int] -> Int ;
length []     := 0 ;
length (x:xs) := 1 + length xs ;

function sumList : [Int] -> Int ;
sumList []     := 0 ;
sumList (x:xs) := x + sumList xs ;

function maximum : [a] -> a ;
maximum (x:[]) := x ;
maximum (x:xs)
    := maximum xs when x < head xs
    := maximum (x:(tail xs)) ;

function minimum : [a] -> a ;
minimum (x:[]) := x ;
minimum (x:xs)
    := minimum xs when x > head xs
    := minimum (x:(tail xs)) ;

function filter : atobool -> [a] -> [a] ;
filter _ [] := [] ;
filter f (x:xs)
    := x:(filter f xs) when (f x)
    := filter f xs ;

function concat : [a] -> [a] -> [a] ;
concat []     ys := ys ;
concat (x:xs) ys := cons x (concat xs ys) ;

function map : atob -> [a] -> [b] ;
map f (x:xs) := (f x):(map f xs) ;
map f []     := [] ;

function foldr : atobtob -> b -> [a] -> b ;
foldr _ b []     := b ;
foldr f b (x:xs) := f x (foldr f b xs) ;

--  folds a list from the left
function foldl : atobtob -> b -> [a] -> b ;
foldl _ a []     := a ;
foldl f a (x:xs) := foldl f (f a x) xs ;

-- Checks if a list of Bools only
-- contains Trues
function and : [Bool] -> Bool ;
and []     := True ;
and (x:xs) := x && and xs ;


-- Checks if a list of Bools only any True
function or : [Bool] -> Bool ;
or []     := False ;
or (x:xs) := x || or xs ;

-- Check if any element of a list
-- satisfies the predicate.
function any : atobool -> [a] -> Bool ;
any f [] := False ;
any f (x:xs)
    := True when f x
    := any f xs ;

-- Check if all element of a list
-- satisfies the predicate.
function all : atobool -> [a] -> Bool ;
all f [] := True ;
all f (x:xs)
    := any f xs when f x
    := False ;

-- misc functions
function id : a -> a ;
id x := x ;
