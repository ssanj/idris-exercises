module Main

import Data.Vect

readToBlank : IO (List String)
readToBlank = do line <- getLine
                 next <- if (line == "") then (pure Nil) else readToBlank
                 pure (line :: next)


readAndSave : IO ()
readAndSave = do input <- readToBlank
                 putStrLn "enter a file name"
                 fileName <- getLine
                 (Right _) <- writeFile fileName (concatMap (++ "\n") input)
                  | Left err => putStrLn (show err)
                 pure ()

readVect : File -> IO (len ** Vect len String)
readVect handle = do  eof <- fEOF handle
                      if (eof) then pure (_ ** [])
                      else do (Right line) <- fGetLine handle
                               | Left err => pure (_ ** [])
                              (_ ** otherLines) <- readVect handle
                              pure (_ ** line :: otherLines)


readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do (Right h) <- openFile filename Read
                           | Left err => pure (_ ** [])
                           content <- readVect  h
                           closeFile h
                           pure content
