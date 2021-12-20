module Sap.Raw.Flag


public export
data Flag
    = Short Char
    | Long String


public export
data FlagParseError
    = ShortNotShort String
    | NoHyphens     String


public export
parseFlag : String -> Either FlagParseError Flag
parseFlag s with (unpack s)
    _ | '-' :: '-' :: xs = Right $ Long $ pack xs
    _ | '-' ::  c  :: [] = Right $ Short c
    _ | '-' ::  c  :: xs = Left  $ ShortNotShort s
    _ | _                = Left  $ NoHyphens s


public export
Show Flag where
    show (Short x) = "-\{show x}"
    show (Long x) = "--\{x}"


public export
Eq Flag where
    Short x == Short y = x == y
    Long  x == Long  y = x == y
    _ == _ = False


public export
Show FlagParseError where
    show (ShortNotShort x) = "Short flags are only one character long"
    show (NoHyphens x) = "Can't parse flag, no hyphens"
