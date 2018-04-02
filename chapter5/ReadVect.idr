module Main

import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do line  <- getLine
                       next  <- readVectLen k
                       pure (line :: next)

data VectUnknown : Type -> Type where
  MkVect : (len: Nat) -> Vect len a -> VectUnknown a


readVect : IO (VectUnknown String)
readVect = do x <- getLine
              if (x == "") then pure (MkVect _ [])
              else do MkVect _ xs <- readVect
                      pure (MkVect _ (x :: xs))


printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) = putStrLn (show xs ++ " (length " ++ show len ++ ")")


anyVect : (n ** Vect n String)
anyVect = (3 ** ["Rod", "Jane", "Freddy"])
