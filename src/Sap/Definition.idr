module Sap.Definition

import Sap.Parsable
import Data.List
import public Data.List.Quantifiers


%default total


public export
record Param where
    constructor (#)
    name : String
    type : Type
    {auto parsable : Parsable type}


public export
record Option where
    constructor MkOption
    short : Char
    long  : String
    types : List Type
    {auto parsables : All Parsable types}


mutual
    public export
    record Command (a : Type) where
        constructor MkCommand
        name : String
        desc : String
        rhs  : RHS a


    public export
    data RHS : Type -> Type where
        SubComm : List (Command a)
               -> RHS a

        Params  : (params  : List Param)
               -> (options : List Option)
               -> (All (.type) params -> All (Maybe . HList . .types) options -> a)
               -> RHS a