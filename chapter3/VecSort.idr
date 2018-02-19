import Data.Vect

insert : Ord elem => (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = if x < y then x :: y :: xs
                              else y :: insert x xs

total inSort : Ord elem => Vect len elem -> Vect len elem
inSort [] = []
inSort (x :: xs) = let xsSorted = inSort xs in
                       insert x xsSorted
