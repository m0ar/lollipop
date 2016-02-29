import sugar ;

datatype Special := False | True ;
datatype Either a b := Left a | Right b ;
datatype Tree a := EmptyTree | Node a (Tree a) (Tree a) ;

--  test iInt ;    DOES NOT WORK!!! For testing of TypeLId

function zeroArguments : Int ;
zeroArguments := 6 ;
-- function zeroArguments : iInt ;   DOES NOT WORK!!! For testing of TypeLId
-- zeroArguments := 6 ;              DOES NOT WORK!!! For testing of TypeLId

function oneArgument : Int -> Int ;
oneArgument 3 := 9 ;
oneArgument a := a + 3 ;

function listArgument : [Int] -> Int ;
listArgument []       := 0 ;
listArgument (x:ys)   := x ;
listArgument (x:y:ys) := y ;
listArgument (x:y:[]) := y ;

function notYetDefined : Bool -> Int ;
notYetDefined b := undefined ;  -- this is a comment

function diffTypes : String -> Char -> Double -> Bool -> Bool ;
diffTypes "hello" 'h' 3.3 True := True && False ;

function biggerThan : Int -> Int -> Bool ;
biggerThan a b := True when a > b
			   := False when a < b
			   := False ;

function swap : (a, b) -> (b, a) ;
swap (a, b) := (b, a) ;

function nestedTuples : (a, (c,d)) -> (a, d) ;
nestedTuples (a, (c,d)) := (a, d) ;

-- Should we allow erronous type identifiers, like False below,
-- in argument type declarations for functions? not just in tuples
-- as below, but for all of them.
-- Gets problematic with custom made types...
function typesInTuples : (String, False) -> Bool ; 
typesInTuples ("hello", False) := True ;

function testListComp : [Int] ;
testListComp := [ x*y | x <- [2,5,10], y <- [8,10,11]] ;