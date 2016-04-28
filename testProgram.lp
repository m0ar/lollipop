datatype Gender := Man a b | Woman Cake | Frog

datatype D a := C (Cake a)

function minz : IO
minz (C x) := case x of
    (Sweet y) -> print "sweet y"
    Lie       -> print "LIE"
    _         -> print "NADA"
minz _     := print "gick inge bra"


function testGen : Gender -> IO
testGen Frog := print "hello"
-- testGen x := case x of
    -- (Woman a) -> print "She's a lady"
    -- (Man a)   -> print "Now, that's a man"
    -- (Frog a)  -> print "A big frog"
    -- _         -> print "Something else"

function testFoldl : Int
testFoldl := foldl (\acc x -> acc + x) 0

function addL : [Int] -> [Int] -> [Int]
great x := x:[2,3,4]

function great : Bool
great := 7 < 8 || 3 > 6

function lett : Int
lett := let x := pw
        in x + 2

function nt : Int
nt := not True

function dv : Int
dv := 5 + 2

function pw : Int
pw := 5 ^ (-2)

function putStrLn : Int
putStrLn := (print "hej" >> print " da, ") >> print " mamma"

function testLam : Int -> Int
testLam := (\y -> 2+(head y)) (tail [1,2,3])

function testLamMap : [Int]
testLam := map (\x -> x+2) [1,2,3]

function sumFst : [(Int,Int)] -> Int
sumFst (x:[]) := fst x
sumFst (x:xs) := (fst x) + (sumFst xs)

function sumFstList : [[Int]] -> Int
sumFstList (x:[]) := head x
sumFstList (x:xs) := (head x) + (sumFstList xs)

function fst : [(Int,Int)] -> Int
fst (x,y) := x

function sum : [Int] -> Int
sum (x:[]) := x
sum (x:xs) := x + sum xs

function head : [Int] -> Int
head (x:[]) := x
head (x:xs) := x

function trd : [Int] -> Int
trd (x:xs) := snd xs

function tail : [Int] -> Int
tail (x:[]) := x
tail (x:xs) := xs

function last : [Int] -> Int
last y := case y of
    (x:[]) -> x
    (x:xs) -> last xs

function lst : [Int]
lst := [True,False,True]

function tpl : (Int, Int)
tpl := (1, 2)

function bl : Bool
bl := True

function tpl2 : (Int, Int) -> Int
tpl2 (1, 2) := 12
tpl2 (x, y) := x-y
tpl2 (3, 4) := 34


function one : Int
one (x,y) := (y,x)

function pat : Int -> Int -> Int -> Int
pat 0 x 2 := 5
pat 0 1 _ := 12
pat _ _ _ := 14

function patR : Int -> Int -> Int -> Int
patR x 2 5 := x + 1
patR y 2 2 := 2

function grd : Int
grd a b := True when a > b
	       := False when a < b
	       := False

function useDbl : Int -> Int
useDbl x := 4 + dbl x

function dbl : Int -> Int
dbl x := case x of
    1 -> 2
    x -> 2 * x

function ca : Int -> Int -> Int -> Int
ca 3 x 2 := case x of
    2 -> 2 + x
    _ -> 10
ca _ 0 0 := 0

function onePat : Int -> Int
pat 2 := 11
pat _ := 0

function testMap : a
testMap := map (print "x") [1,2,3]

function naturals : [Int]
naturals := cons 0 (map (\x -> x+1) naturals)

-- Sum of first 100 naturals
function main : Int
main := foldr (\x -> \y -> (x+y)) 0 (take 5 [1,2,3,4,5,6,7,8])

function testPat : Int -> [Int] -> [Int]
testPat := pat 0 dv 2

function lazy : Int
lazy := oneFsc 5 infy

function oneFsc : Int
oneFsc x y := x

function infy : Int
infy := 1 + infy
