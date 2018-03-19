module Main

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

readNumbers : IO (Maybe (Nat, Nat))
readNumbers =
  do Just num1_ok <- readNumber
     Just num2_ok <- readNumber
     pure (Just (num1_ok, num2_ok))


readNumbers2 : IO (Maybe (Nat, Nat))
readNumbers2 =
          do Just num1_ok <- readNumber | Nothing => pure Nothing
             Just num2_ok <- readNumber | Nothing => pure Nothing
             pure (Just (num1_ok, num2_ok))
