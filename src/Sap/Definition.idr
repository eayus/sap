module Sap.Definition

import Sap.Parsable
import Data.List
import public Data.List.Quantifiers
import public Control.Monad.Identity
import public Control.Monad.Writer
import public Control.Monad.Either
import public Control.Monad.Trans


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
        constructor Cmd
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


public export
record Help where
    constructor MkHelp
    0 ty : Type
    cmdPath : List String
    cmd : Command ty


public export
data Failure
    = HelpFor Help
    | Error String


public export
Result : Type -> Type
Result = EitherT Failure (Writer (List String))
