import Data.Vect

-- vectTake : (n: Nat) -> Vect (S n) a -> Vect n a
vectTake : (n: Nat) -> Vect (n + m) a -> Vect n a
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs
