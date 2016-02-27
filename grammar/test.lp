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

function diffTypes : String -> Char -> Double -> Bool ;
diffTypes "hello" 'h' 3.3 := True && False ;

function bigger : Int -> Int -> Bool ;
bigger a b := True when a > b
		   := True when a < b
		   := False ;