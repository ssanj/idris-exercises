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

processInput : DataStore -> String -> Maybe (String, DataStore)


main : IO ()
main = replWith (MkData _ []) "Command: " ?processInput
