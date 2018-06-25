module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

-- data DataStore : Type where
--   MkData : (schema: Schema) -> (size: Nat) -> (items: Vect size (SchemaType schema)) -> DataStore

record DataStore where
        constructor MkData
        schema: Schema
        size: Nat
        items: Vect size (SchemaType schema)

addToStore : (store: DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newitem = MkData schema _ (addToData store)
  where
    addToData : Vect oldsize (SchemaType schema) -> Vect (S oldsize) (SchemaType schema)
    addToData [] = [newitem]
    addToData (item :: items) = item :: addToData items


display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

getEntry : (pos : Integer) -> (store : DataStore) ->
           Maybe (String, DataStore)
getEntry pos store
  = let store_items = items store in
        case integerToFin pos (size store) of
          Nothing => Just ("Out of range\n", store)
          Just id => Just (display (index id (items store)) ++ "\n", store)

data Command : Schema -> Type where
       Add : SchemaType schema -> Command schema
       Get : Integer -> Command schema
       -- Size
       -- Search String
       Quit : Command schema

parseCommand : (schema: Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" rest = case parseBySchema schema rest of
                                    Nothing => Nothing
                                    Just restok => Just (Add restok)

parseCommand schema "get" val = case all isDigit (unpack val) of
                            True => Just (Get (cast val ))
                            False => Nothing
-- parseCommand "size" "" = Just Size
-- parseCommand "search" str = Just (Search str)
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _       = Nothing

parse : (schema: Schema) -> (input: String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                       (cmd, args) => parseCommand schema cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
  = case parse (schema store) input of
      Nothing => Just ("Invalid command\n", store)
      Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
      Just (Get pos) => getEntry pos store
      Just Quit => Nothing

parseBySchema : (schema: Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = Just (?rhs_schema)

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)


main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
