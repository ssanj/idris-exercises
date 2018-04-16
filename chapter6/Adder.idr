module Main

AdderType : (n: Nat) -> Type
AdderType Z = Int
AdderType (S k) = (next: Int) -> AdderType k

adder : (numArgs: Nat) -> (acc: Int) -> AdderType numArgs
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)


AdderType2 : (numArgs: Nat) -> Type -> Type
AdderType2 Z numType = numType
AdderType2 (S k) numType = (next: numType) ->  AdderType2 k numType

adder2 : Num numType => (numArgs: Nat) -> numType -> AdderType2 numArgs numType
adder2 Z acc = acc
adder2 (S k) acc = \next => adder2 k (next + acc)
