module Main

import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

countdown : (secs: Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S secs) = do putStrLn (show (S secs))
                        usleep 1000000
                        countdown  secs

countdowns : IO ()
countdowns = do putStr "Enter starting number: "
                Just startNum <- readNumber
                    | Nothing => do putStrLn "Invalid input"
                                    countdowns

                countdown startNum
                putStr "Another (y/n)? "
                yn <- getLine
                if yn == "y" then countdowns
                else pure ()

guess : (target: Nat) -> IO ()
guess x = do putStrLn "Please enter a number"
             Just guessed <- readNumber
             | Nothing => do putStrLn "Invalid input: "
                             guess x

             if guessed == x then putStrLn "you guessed it!"
             else if guessed > x then do putStrLn "too high"
                                         guess x
             else do putStrLn "too low"
                     guess x

guess2 : (target: Nat) -> (guesses: Nat) -> IO ()
guess2 x count = do putStrLn ("number of guesses: " ++ show count)
                    putStrLn "Please enter a number"
                    Just guessed <- readNumber
                    | Nothing => do putStrLn "Invalid input: "
                                    guess2 x (count + 1)

                    if guessed == x then putStrLn "you guessed it!"
                    else if guessed > x then do putStrLn "too high"
                                                guess2 x (count + 1)
                    else do putStrLn "too low"
                            guess2 x (count + 1)


main : IO ()
main = do
        num <- time
        let target = num `mod` 100
        _ <- guess2 (fromIntegerNat target) 0
        pure ()
