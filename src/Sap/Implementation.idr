module Sap.Implementation

import Data.List
import Sap.Definition
import Sap.Parsable
import Sap.Util


%default total


-- Preprocessing


data Chunk
    = CRaw   String
    | CShort Char
    | CLong  String


toChunk : String -> Error Chunk
toChunk s with (unpack s)
    _ | '-' :: '-' :: rest = pure $ CLong $ pack rest
    _ | ['-', c]           = pure $ CShort c
    _ | '-' :: rest        = Left "Short arguments are only one letter long."
    _ | _                  = pure $ CRaw s


chunkify : List String -> Error (List Chunk)
chunkify = traverse toChunk


data Part
    = PRaw   String
    | PShort Char   (List String)
    | PLong  String (List String)



-- For this, look at how "splitOn" functions are implemented.
-- like, "ab, fds, wq".split(',')
parts : List Chunk -> List Part
parts []               = []
parts (CRaw x   :: xs) = PRaw x :: parts xs
parts (CShort x :: xs) = ?h_1
parts (CLong x  :: xs) = ?h_2




{-
data Chunk
    = Flag String
    | Name String


chunkify : String -> Chunk
chunkify s with (unpack s)
    _ | '-' :: '-' :: cs = Flag $ pack cs
    _ |        '-' :: cs = Flag $ pack cs
    _ | _                = Name s


defaultOptions : {options : _} -> All (Maybe . Param.type) options
defaultOptions = allTautology $ \_ => Nothing


parseParams : List Chunk -> (params : List Param) -> (options : List Param)
           -> Error (All (Param.type) params, All (Maybe . Param.type) options)
parseParams []             []        options = pure ([], defaultOptions)
parseParams []             (y :: ys) options = Left "Expected parameter \{y.name}"
parseParams (     x :: xs) []        options = ?h_3
parseParams (Flag x :: xs) (y :: ys) options = Left "Encountered flag \{x} but more arguments are still expected (please put flags at the end)"
parseParams (Name x :: xs) (y :: ys) options = do
    let p = y.parsable
    x' <- parse x
    (ps, os) <- parseParams xs ys options
    pure (x' :: ps, os)



parseCommand' : List Chunk -> Command a -> Error a
parseCommand' chunks cmd with (cmd.rhs)
    _ | Params params options run = uncurry run <$> parseParams chunks params options

    parseCommand' []             cmd | SubComm cmds = Left "Expected sub command"
    parseCommand' (Flag x :: xs) cmd | SubComm cmds = Left "Unexpected flag \{x}, expected sub command"
    parseCommand' (Name x :: xs) cmd | SubComm cmds =
        case find (\c => x == c.name) cmds of
            Just c  => parseCommand' xs c
            Nothing => Left "Invalid subcommand \{x}"


parseCommand : List String -> Command a -> Error a
parseCommand = parseCommand' . map chunkify-}