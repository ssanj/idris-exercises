module Main

import Data.Vect

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon n = Vect n Position

StringVect : Nat -> Type
StringVect n = Vect n String

moarString : StringVect 5
moarString = ["one", "two", "three", "four", "five"]

tri : Polygon 3
tri = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]
