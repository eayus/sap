module Sap.Definition

import Sap.Parsable
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

public export 0
Args : List Param -> Type -> Type
Args [] r = r
Args (p :: ps) r = Arg p -> Args ps r

public export
data Opts : List Option -> Type where
    Nil : Opts []
    (::) : Maybe (All Arg opt.params) -> Opts opts -> Opts (opt :: opts)

public export
getOpt :
    (0 opt : Option) ->
    {auto elem : Elem opt opts} ->
    Opts opts ->
    Maybe (All Arg opt.params)
getOpt opt {elem = Here} (x :: _) = x
getOpt opt {elem = (There elem)} (_ :: xs) = getOpt opt {elem} xs

public export
optAt :
    (n : Nat) ->
    {auto ib : InBounds n opts} ->
    Opts opts ->
    Maybe (All Arg (index n opts @{ib}).params)
optAt 0 {ib = InFirst} (x :: xs) = x
optAt (S k) {ib = (InLater ib)} (x :: xs) = optAt k xs

public export
getParam :
    {0 params : List Param} ->
    (n : Nat) ->
    {auto ib : InBounds n params} ->
    All Arg params ->
    (index n params @{ib}).type
getParam Z {ib = InFirst} (x :: _) = x
getParam (S n) {ib = InLater ib} (_ :: xs) = getParam n xs

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
               -> (Args params (Opts options -> a))
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
