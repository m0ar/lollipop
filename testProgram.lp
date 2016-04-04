function great : Bool ;
great := 7 < 8 || 3 > 6 ;

function dv : Int ;
dv := -5 - -2 ;

function pw : Int ;
pw := 5 ^ (-2) ;

function putStrLn : Int ;
putStrLn := (print "hej" >> print " da, ") >> print " mamma"  ;

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
    (x:[]) -> x ;
    (x:xs) -> last xs ;;

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
grd a b := True when a > b
	       := False when a < b
	       := False ;

function useDbl : Int -> Int ;
useDbl x := 4 + dbl x ;

function dbl : Int -> Int ;
dbl x := case x of
    1 -> 2 ;
    x -> 2 * x ;;

function ca : Int -> Int -> Int -> Int ;
ca 3 x 2 := case x of
    2 -> 2 + x ;
    _ -> 10 ;;
ca _ 0 0 := 0 ;

function onePat : Int -> Int ;
pat 2 := 11 ;
pat _ := 0 ;

function take : Int -> [a] -> [a] ;
take n (x:xs) := case n of
    0 -> cons x (take (n-1) xs) ;
    _ -> nil ; ;

take n [] := nil ; -- There is no way to do empty lists :(

function map : atob -> [a] -> [b] ;
map f (x:xs) := cons (f x) (map f xs) ;
map f [] := nil ;

function naturals : [Int] ;
naturals := cons 0 (map (\x -> x+1) naturals) ;

function foldr : atobtob -> b -> [a] -> b ;
foldr f b [] := b ;
foldr f b (x:xs) := f x (foldr f b xs) ;

-- Sum of first 100 naturals
function main : Int ;
main := foldr (\x -> \y -> (x+y)) 0 (take 100 naturals) ;
