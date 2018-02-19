module Main

import Data.Vect

data DataStore : Type where
  MkData : (size: Nat) ->
           (items: Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MkData size' _) = size'

items : (store: DataStore) -> Vect (size store) String
items (MkData _ items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size' items') newItem = MkData _ (addToData items')
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (item :: items) = item :: addToData items

data Command = Add String
             | Get Integer
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                            True => Just (Get (cast val ))
                            False => Nothing
parseCommand "quit" "" = Just Quit
parseCommand _ _       = Nothing

-- why do we need to unpack the String to run all on it?
-- in haskell we can just do: all isDigit "12345"

parse : (input: String) -> Maybe Command
parse input = case span (/= ' ') input of
                (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (idx : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store@(MkData size' items') = case integerToFin pos size' of
                                                 Nothing => Just ("Out of range\n", store)
                                                 Just (idx) => Just (index idx items' ++ "\n", store)

processCommand : (command : Command) -> (store : DataStore) ->  Maybe (String, DataStore)
processCommand (Add item) store = Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
processCommand (Get idx) store = getEntry idx store
processCommand Quit _ = Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                          Just (command) => processCommand command store
                          Nothing => Just ("Invalid command\n", store)



main : IO ()
main = replWith (MkData _ []) "Command: " processInput
