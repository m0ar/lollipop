-- ------------------------------------------
-- DataTypes
-- ------------------------------------------

-- Works like Haskells Maybe
datatype Cake a := Sweet a | Lie
-- either the cake is a Sweet or
-- the cake is a lie

datatype Undefined := Undefined

function getCake : Cake -> a
getCake (Sweet x) := x
getCake _         := Undefined

datatype Boolean := True | False

-- ------------------------------------------

-- misc functions
function id : a -> a
id x := x

-- ------------------------------------------
-- Mathematical functions
-- ------------------------------------------

-- The factorial function
function fac : a -> b
fac 0 := 1
fac x := x * (fac (x-1))

-- Calculates the n:th Fibonacci number
function fib : Int -> Int
fib 0 := 0
fib 1 := 1
fib n := fib (n-2) +
         fib (n-1)

-- Calculates the absolute value
function abs : Int -> Int
abs x
    := -x when x < 0
    := x

-- Returns the "sign" for integers (1 for
-- numbers >= 0 and -1 for numbers < 0)
function sign : Int -> Int
sign x
    := -1 when x < 0
    := 1

-- Integer division
function div : Int -> Int -> Int
div _ 0 := Undefined
div 0 _ := 0
div x y
    := div -x -y        when y < 0
    := -1 + div (x+y) y when x < 0
    := 1 + div (x-y) y  when x > y
    := 1                when x == y
    := 0

-- Modulus
function mod : Int -> Int -> Int
mod x y := x - y * div x y

-- Returns the successor of an integer
function succ : Int -> Int
succ x := x + 1

-- Returns the predecessor of an integer
function pred : Int -> Int
pred x := x - 1

-- Calculates the greatest common divisor
-- of the two arguments
function gcd : Int -> Int -> Int
gcd x 0 := abs x
gcd x y := gcd y (mod x y)

-- Calculates the least common multiple
-- of the two arguments
function lcm : Int -> Int -> Int
lcm 0 0 := 0
lcm x y := div (abs (x*y)) (gcd x y)

-- ------------------------------------------
-- Logical functions
-- ------------------------------------------

-- Returns true if argument is odd
function isOdd : Int -> Boolean
isOdd x := mod x 2 == 1

-- Returns true if argument is even
function isEven : Int -> Boolean
isEven x := mod x 2 == 0

-- Checks if a list of Booleans only
-- contains Trues
function and : [Boolean] -> Boolean
and := foldr (\x y -> x && y) True

-- Checks if a list of Booleans contains
-- any True
function or : [Boolean] -> Boolean
or xs := foldr (\x y -> x || y) False xs

-- Check if any element of a list
-- satisfies the predicate.
function any : atoBoolean -> [a] -> Boolean
any f xs := or (map f xs)

-- Check if all element of a list
-- satisfies the predicate.
function all : atoBoolean -> [a] -> Boolean
all f xs := and (map f xs)

-- ------------------------------------------
-- Functions over tuples
-- ------------------------------------------

-- Returns the first element in the tuple
-- function fst : (a,b) -> a
-- fst (x,y) := x

-- -- Returns the second element in the tuple
-- function snd : (a,b) -> b
-- snd (x,y) := y

-- Returns the maximum of the two elements
-- in the tuple
-- function max : (a,a) -> a
-- max (x,y)
--     := x when x > y
--     := y

-- Returns the minimum of the two elements
-- in the tuple
-- function min : (a,a) -> a
-- min (x,y)
--     := x when x < y
--     := y

-- ------------------------------------------
-- Functions over lists
-- ------------------------------------------

-- Returns the first elements in the list
function head : [a] -> a
head []     := Undefined
head (x:xs) := x

-- Returns the list of all elements except
-- for the last in the input list
function init : [a] -> [a]
init []     := Undefined
init (x:[]) := []
init (x:xs) := x:(init xs)

-- Returns the list of all elements except
-- for the first in the input list
function tail : [a] -> [a]
tail []     := Undefined
tail (x:xs) := xs

-- Returns the last elements in the list
function last : [a] -> a
last []     := Undefined
last (x:[]) := x
last (x:xs) := last xs

-- Returns the n first elements of the list
function take : Int -> [a] -> [a]
take n []     := []
take 0 _      := []
take n (x:xs) := x:(take (n-1) xs)

-- Returns the list consisting of all elements
-- that occur before the first element failing
-- the predicate
function takeWhile : (a -> Boolean) -> [a] -> [a]
takeWhile _ [] := []
takeWhile f (x:xs)
    := (x:(takeWhile f xs)) when f x
    := []

-- Returns all except the n first elements of
-- the list
function drop : Int -> [a] -> [a]
drop _ []     := []
drop 0 xs     := xs
drop n (x:xs) := drop (n-1) xs

-- Returns the list consisting of all elements
-- that occur after the first element failing
-- the predicate (including that element)
function dropWhile : (a -> Boolean) -> [a] -> [a]
dropWhile _ [] := []
dropWhile f (x:xs)
    := dropWhile f xs when f x
    := (x:xs)

-- Returns the length of the list
function length : [a] -> Int
length []     := 0
length (x:xs) := 1 + length xs

-- Returns the reversed list
-- function reverse : [a] -> [a]
-- reverse []     := []
-- reverse (x:xs) := reverse xs ++ [x]

-- Returns true if the given value is an element
-- in the list, otherwise false.
function elem : [a] -> a -> Boolean
elem xs := any (\x y -> x==y) xs

-- Returns the sum of all elements of the list
function sumList : [a] -> a
sumList := foldr (\x y -> x+y) 0

-- Returns the greatest element of the list
function maximum : [a] -> a
maximum (x:[]) := x
maximum (x:xs)
    := maximum xs when x < head xs
    := maximum (x:(tail xs))

-- Returns the smallest element of the list
function minimum : [a] -> a
minimum (x:[]) := x
minimum (x:xs)
    := minimum xs when x > head xs
    := minimum (x:(tail xs))

-- Returns the elements satisfying the
-- predicate
function filter : atoBoolean -> [a] -> [a]
filter _ [] := []
filter f (x:xs)
    := x:(filter f xs) when (f x)
    := filter f xs

function concat : [a] -> [a] -> [a]
concat []     ys := ys
concat (x:xs) ys := x:(concat xs ys)

function map : atob -> [a] -> [b]
map f (x:xs) := (f x):(map f xs)
map f []     := []

function foldr : atobtob -> b -> [a] -> b
foldr _ b []     := b
foldr f b (x:xs) := f x (foldr f b xs)

-- Folds a list from the left
function foldl : atobtob -> b -> [a] -> b
foldl _ a []     := a
foldl f a (x:xs) := foldl f (f a x) xs

-- Zips together two lists to a list of tuples
function zip : [a] -> [b] -> [(a,b)]
zip [] _          := []
zip _ []          := []
zip (x:xs) (y:ys) := (x,y):(zip xs ys)

-- Unzips a list of tuples to a tuple of lists
-- function unzip : [(a,b)] -> ([a], [b])
-- unzip []          := ([], [])
-- unzip ((x,y):xys) := ((x:(fst (unzip xys))),
--                       (y:(snd (unzip xys))))

-- Applies the function on the corresponding elements
-- in the input lists and returns the list of the
-- values returned by the function
-- function zipWith : (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith _ [] _          := []
-- zipWith _ _ []          := []
-- zipWith f (x:xs) (y:ys) := (f x y):(zipWith f xs ys)

-- print takes a list of characters and calls printChar one every element
function print : [Char] -> IO ()
print []     := printChar '\n'
print (c:cs) := printChar c >> print cs