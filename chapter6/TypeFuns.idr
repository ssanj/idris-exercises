module Main

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True  = Int

getStringOrInt : (isInt: Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety Four"
getStringOrInt True = 94

valToString : (isInt: Bool) -> StringOrInt isInt -> String
valToString False = trim
valToString True = cast

valToString2 : (isInt: Bool) -> (case isInt of
                                      False => String
                                      True => Int) -> String
valToString2 False = trim
valToString2 True = cast
