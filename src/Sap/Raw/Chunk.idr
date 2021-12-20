module Sap.Raw.Chunk

import Sap.Raw.Flag


public export
data Chunk
    = FlagChunk Flag
    | ArgChunk  String


toChunk : String -> Either FlagParseError Chunk
toChunk s with (parseFlag s)
  _ | Left (NoHyphens x) = Right $ ArgChunk s
  _ | Left err           = Left err
  _ | Right x            = Right $ FlagChunk x


export
chunkify : List String -> Either FlagParseError (List Chunk)
chunkify = traverse toChunk
