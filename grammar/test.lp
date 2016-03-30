-- Test for the import keyword
import sugar ;

-- Test for algebraic datatypes
datatype Special := This | That ;
datatype iSpecial := iThis | iThat ;
datatype Either a b := Left a | Right b ;
datatype Tree a := EmptyTree | Node a (Tree a) (Tree a) ;

-- Test for deriving
datatype Point := Point Float Float deriving (Show) ;
datatype Day := Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
           deriving (Eq, Ord, Show, Read, Bounded, Enum) ;

-- Test for type synonyms
type String := [Char] ;
type AssocList k v := [(k,v)] ;
type IntMap v := Map Int v ;

-- Test for higher order function
function map : (a -> b) -> [a] -> [b];
map f (x:xs) := cons(f x, map f xs) ;


-- Test for a function with zero arguments
function zeroArguments : Int ;
zeroArguments := 6 ;

-- Test for a function with one argument
function oneArgument : Int -> Int ;
oneArgument 3 := 9 ;
oneArgument a := aa + 3 ;

-- Test for undefined function
function notYetDefined : Bool -> Int ;
notYetDefined b := undefined ;  -- this is a comment

-- Test for different datatypes as arguments
function diffTypes : String -> Char -> Double -> Bool -> Bool ;
diffTypes "hello" 'h' 3.3 True := True && False ;

-- Test function with linear types
function linear : iString -> iString ;
linear s := s ;

-- Test for pattern matching of lists
function listArgument : [Int] -> Int ;
listArgument []       := 0 ;
listArgument (x:ys)   := x ;
listArgument (x:y:ys) := y ;
listArgument (x:y:[]) := y ;

-- Another test for pattern matching of lists
function head : [a] -> a ;
head (x:_) := x ;
head []	   := error ;

-- Test for guards
function biggerThan : Int -> Int -> Bool ;
biggerThan a b := True when a > b
	       := False when a < b
	       := False ;
biggerThan a b := case a > b of 
                    5  -> true ; 
                    _ -> false ;; 

-- Test for tuples with identifiers as arguments
function swap : (a, b) -> (b, a) ;
swap (a, b) := (b, a) ;

-- Test for nested tuples
function nestedTuples : (a, (c,d)) -> (a, d) ;
nestedTuples (a, (c,d)) := (a, d) ;
nestedTuples (c, (d,a)) := (d, (a,c)) ;

-- Test for tuples with datatypes as arguments
function typesInTuples : (String, Bool) -> Bool ; 
typesInTuples ("hello", False) := True ;

-- Test for lists as expressions
function names : [a] ;
names := ["Blick", "Flick", "Glick", "Plick", "Quee", "Snick", "Whick"] ;

-- Test for only input sets in list comprehensions
function testListComp : [Int] ;
testListComp := [ x*y | x <- [2,5,10], y <- [8,10,11]] ;

-- Test for both input set(s) and predicates in list comprehensions
function testListComp2 : [Bool] ;
testListComp2 := [ x == y | x <- ['a','b','c'], y <- ['c','a','f'], x != 'b', y != 'f'] ;

-- Test of '_' in list comprehensions
function length' : Int ;
length' xs := sum [1 | _ <- xs] ;

-- Test of let bindings
function cylinder : a -> a -> a ;
cylinder r h := 
    let sideArea := 2 * pi * r * h ;
        topArea := pi * r^2 ;
    in  sideArea + 2 * topArea ; ;

-- Test for linear types
function oneArgument : iInt -> iInt ;
oneArgument a := a + 3 ;

-- Test for enclosed expressions
function encl : Int ;
encl := (3 + (6 + (factorial 9 + (6-3)))) ;

-- test empty expression
function testEmpExp : [a] ;
testEmpExp := [] ;

function sumFst : [(Int,Int)] -> Int ;
sumFst ((1,2):vs) := 1 ;
sumFst (X:[]) := fst x ;