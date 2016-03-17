function one : Int ;
one := 1 ;

function pat : Int -> Int -> Int -> Int;
pat 0 9 2 := 10 ;
pat _ 1 2 := 12 ;
pat _ _ _ := 14 ;

function take : Int -> [a] -> [a] ;
take n (x:xs) := case n of 0 -> cons x (take (n-1) xs) ;
_ -> nil ;
;
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
