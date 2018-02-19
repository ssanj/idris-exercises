module HOC

double : Num ty => ty -> ty
double x = x + x

twice : (a -> a) -> a -> a
twice f x = f (f x)

Shape: Type
rotate: Shape -> Shape

quadrupal : Num a => a -> a
quadrupal = twice double

turn_around : Shape -> Shape
turn_around = twice rotate
