import sugar ;

datatype Tree a := EmptyTree | Node a (Tree a) (Tree a) ;
datatype Int := False | True ;
datatype Either a b := Left a | Right b ;

function f : Int ;
f := 6 ;

function g : Int -> Int ;
g 3 := 9 ;
g a := a + 3 ;
g' (x:ys) := x ;
g (x:y:ys) := y ;
g (x:y:[]) := y ;

function h : Bool -> Int ;
h b := undefined ;  -- TODO

function zulu : String -> Char -> Double ;
zulu hello h := 3.3 ;