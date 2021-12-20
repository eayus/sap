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
    short  : Char
    long   : String
    params : List Param


public export
Arg : Param -> Type
Arg param = param.type


mutual
    public export
    record Command (a : Type) where
        constructor MkCommand
        name : String
        desc : String
        rhs  : RHS a


    public export
    data RHS : Type -> Type where
        SubCmds : List (Command a)
               -> RHS a

        Basic   : (params  : List Param)
               -> (options : List Option)
               -> (All Arg params -> All (Maybe . All Arg . .params) options -> a)
               -> RHS a
