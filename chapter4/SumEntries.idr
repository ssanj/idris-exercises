import Data.Vect


sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries pos [] [] = Nothing
sumEntries {n} pos xs ys = case integerToFin pos n of
                              Nothing => Nothing
                              (Just idx) => Just (index idx xs + index idx ys)
