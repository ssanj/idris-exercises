import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

createEmpties2 : Vect n (Vect 0 elem)
createEmpties2 {n = Z} = []
createEmpties2 {n = (S k)} = [] :: createEmpties2

transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             transposeHelper x xsTrans
