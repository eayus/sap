module Sap.Definition

import Sap.Parsable
import Sap.Util
import Data.List
import public Data.List.Elem
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


public export
Args : List Param -> Type
Args = All Arg


public export
Opt : Option -> Type
Opt o = Maybe (Args o.params)


public export
Opts : List Option -> Type
Opts = All Opt


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
               -> (Args params -> Opts options -> a)
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



public export
simpleOpt : (name : String) -> (a : Type) -> {auto _ : Parsable a} -> {auto ne : NonEmpty (unpack name)} -> Option
simpleOpt name ty with (unpack name)
  simpleOpt name ty | (c :: cs) = MkOption { short = c, long = name, params = [name # ty] }


public export
indexArgs : (n : Nat) -> {auto 0 ib : InBounds n params} -> Args params -> Arg (index n params)
indexArgs = indexAll


public export
argByName : (0 name : String) -> Args params -> {auto elem : Any (\x => x.name = name) params} -> Arg (getAny elem)
argByName name = allByProp (\x => x.name = name)


public export
optByShort : (0 short : String) -> Opts options -> {auto elem : Any (\x => x.short = short) options} -> Opt (getAny elem)
optByShort short = allByProp (\x => x.short = short)


public export
optByLong : (0 long : String) -> Opts options -> {auto elem : Any (\x => x.long = long) options} -> Opt (getAny elem)
optByLong long = allByProp (\x => x.long = long)

