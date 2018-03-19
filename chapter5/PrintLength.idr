module Main

printLength : IO ()
printLength = putStrLn "Input string: " >>= \_ =>
              getLine >>= \input => let len = length input in
                putStrLn (show len)

printLength2 : IO ()
printLength2 = do
                putStrLn "Input string: "
                line <- getLine
                let len = length line
                putStrLn (show len)


printLonger : IO ()
printLonger = do
                putStrLn "input1: "
                line1 <- getLine
                putStrLn "input2: "
                line2 <- getLine
                let len1 = length line1
                let len2 = length line2
                putStrLn ("longer: " ++ (show $ max len1 len2))
