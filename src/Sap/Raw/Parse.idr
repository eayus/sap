module Sap.Raw.Parse

import Sap.Raw.Chunk
import Sap.Raw.Flag


public export
OptMap : Type
OptMap = List (Flag, List String)


public export
record ParsedInputs where
    constructor MkParsedInputs
    args : List String
    opts : OptMap


eatArgs : List Chunk -> (List String, List Chunk)
eatArgs [] = ([], [])
eatArgs xs@(FlagChunk _ :: _) = ([], xs)
eatArgs (ArgChunk x :: xs) =
    let (args, rest) = eatArgs xs in (x :: args, rest)


-- Requires that the chunks do not begin with an arg
parseOpts : List Chunk -> OptMap
parseOpts [] = []
parseOpts (ArgChunk  x :: xs) = believe_me ()
parseOpts (FlagChunk x :: xs) =
    let (args, rest) = eatArgs xs
    in (x, args) :: assert_total (parseOpts rest)


export
parseInputs : List Chunk -> ParsedInputs
parseInputs xs =
    let (args, rest) = eatArgs xs
        opts = parseOpts rest
    in MkParsedInputs { args = args, opts = opts }
