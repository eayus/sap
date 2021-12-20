module Sap.Implementation

import Data.List
import Sap.Definition
import Sap.Parsable
import Sap.Util


%default total

data OptionKey
    = Short Char
    | Long String


Show OptionKey where
    show (Short x) = "-\{show x}"
    show (Long x) = "--\{x}"


Eq OptionKey where
    (Short x) == (Short y) = x == y
    (Short x) == (Long y) = False
    (Long x) == (Short y) = False
    (Long x) == (Long y) = x == y


-- Preprocessing


data Chunk
    = CRaw String
    | COpt OptionKey


toChunk : String -> Error Chunk
toChunk s with (unpack s)
    _ | '-' :: '-' :: rest = pure $ COpt $ Long $ pack rest
    _ | ['-', c]           = pure $ COpt $ Short c
    _ | '-' :: rest        = Left "Short arguments are only one letter long."
    _ | _                  = pure $ CRaw s


chunkify : List String -> Error (List Chunk)
chunkify = traverse toChunk


record ParsedArgs where
    constructor MkParsedArgs
    inputs  : List String
    options : Map OptionKey (List String)


eatRaws : List Chunk -> (List String, List Chunk)
eatRaws [] = ([], [])
eatRaws (CRaw x :: xs) = let (raws, rest) = eatRaws xs in (x :: raws, rest)
eatRaws xs@(COpt _ :: _) = ([], xs)


parseOpts : List Chunk -> Map OptionKey (List String)
parseOpts [] = []
parseOpts (COpt x :: xs) =
    let (args, rest) = eatRaws xs
    in (x, args) :: assert_total (parseOpts rest)
parseOpts (CRaw x :: xs) = ?parseOpts_rhs_2 -- Impossible


parseArgs : List Chunk -> ParsedArgs
parseArgs xs = let (inputs, rest) = eatRaws xs
               in MkParsedArgs inputs (parseOpts rest)


parseParam : String -> (param : Param) -> Error (param.type)
parseParam x param = let _ = param.parsable in parse x


parseParams : List String -> (params : List Param)
           -> Error (All (.type) params)
parseParams [] [] = pure []
parseParams [] (x :: xs) = Left "Expected more arguments"
parseParams (x :: xs) [] = Left "Unexpected argument \{x}"
parseParams (x :: xs) (y :: ys) = do
    z <- parseParam x y
    zs <- parseParams xs ys
    pure $ z :: zs


lookupOpt : Map OptionKey (List String) -> (short : Char) -> (long : String)
         -> Maybe (List String)
lookupOpt optMap short long =
    case lookup (Short short) optMap of
         Just xs => pure xs
         Nothing => case lookup (Long long) optMap of
                         Just xs => pure xs
                         Nothing => Nothing

convertOpt : List String -> (opt : Option) -> Error (HList opt.types)
convertOpt xs opt = go xs opt.types opt.parsables
    where
        go : List String -> (types : List Type)
          -> (parsable : All Parsable types) -> Error (HList types)
        go [] [] parsable = pure []
        go [] (x :: ys) parsable = Left $ "Option \{opt.long} expects more arugments"
        go (x :: xs) [] parsable = Left $ "Option \{opt.long} recieved too many arguments"
        go (x :: xs) (y :: ys) (z :: zs) = do
            w <- parse x
            ws <- go xs ys zs
            pure $ w :: ws


-- TODO, warn about unknown arguments
convertOptions : Map OptionKey (List String) -> (options : List Option)
              -> Error (All (Maybe . HList . .types) options)
convertOptions optMap [] = pure []
convertOptions optMap (x :: xs) =
    case lookupOpt optMap x.short x.long of
         Just rhs => do
            rhs' <- convertOpt rhs x
            zs <- convertOptions optMap xs
            pure $ Just rhs' :: zs
         Nothing  => do
            zs <- convertOptions optMap xs
            pure $ Nothing :: zs


convertArgs : ParsedArgs -> (params : List Param) -> (options : List Option)
          -> Error (All (.type) params, All (Maybe . HList . .types) options)
convertArgs x params options = do
    l <- parseParams x.inputs params
    r <- convertOptions x.options options
    pure (l, r)




parseCommand' : List Chunk -> Command a -> Error a
parseCommand' chunks cmd with (cmd.rhs)
    _ | Params params options run = do
          let pargs = parseArgs chunks
          (l, r) <- convertArgs pargs params options
          pure $ run l r

    parseCommand' []             cmd | SubComm cmds = Left "Expected sub command"
    parseCommand' (COpt x :: xs) cmd | SubComm cmds = Left "Unexpected opt \{show x}, expected sub command"
    parseCommand' (CRaw x :: xs) cmd | SubComm cmds =
        case find (\c => x == c.name) cmds of
            Just c  => parseCommand' xs c
            Nothing => Left "Invalid subcommand \{x}"


export
parseCommand : List String -> Command a -> Error a
parseCommand xs cmd = do
    xs' <- chunkify xs
    parseCommand' xs' cmd
