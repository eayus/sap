module Sap

import public Data.List.Quantifiers
import Decidable.Equality

%default total


-- Command definition

public export
data ArgType
    = Str


public export
data Command : Type where
    Child  : (args : List (String, ArgType))
          -> Command

    Parent : (subCommands : List (String, Command))
          -> Command


-- Handler definition

public export
Embed : ArgType -> Type
Embed Str = String


public export
data Named : String -> Type where
    Name : (name : String) -> Named name


public export
data Handler : Command -> (retType : Type) -> Type where
    HChild  : {a : Type}
           -> (All (\p => (Named (fst p), Embed (snd p))) args -> a)
           -> Handler (Child args) a

    HParent : {a : Type}
           -> All (\p => (Named (fst p), Handler (snd p) a)) subCommands
           -> Handler (Parent subCommands) a


-- Parsing implementation


parseArg : (input : String) -> (arg : (String, ArgType)) -> Maybe (Named (fst arg), Embed (snd arg))
parseArg input (name, Str) = Just (Name name, input)


parseArgs : (inputs : List String) -> (args : List (String, ArgType)) -> Maybe (All (\p => (Named (fst p), Embed (snd p))) args)
parseArgs [] [] = Just []
parseArgs (x :: xs) (y :: ys) = [| parseArg x y :: parseArgs xs ys |]
parseArgs _ _ = Nothing


lookupAll : DecEq a => {P : (a, b) -> Type} -> (x : a) -> (xs : List (a, b)) -> All P xs -> Maybe (y : b ** P (x, y))
lookupAll x [] [] = Nothing
lookupAll x ((y, y') :: ys) (z :: zs) with (decEq x y)
    lookupAll x ((x, y') :: ys) (z :: zs) | Yes Refl = Just (MkDPair y' z)
    lookupAll x ((y, y') :: ys) (z :: zs) | No neq   = lookupAll x ys zs


export
runHandler : List String -> (command : Command) -> Handler command a -> Maybe a
runHandler inputs (Child args) (HChild f) = f <$> parseArgs inputs args
runHandler (sub :: inputs) (Parent subCommands) (HParent subHandlers) =
    case lookupAll sub subCommands subHandlers of
        Just (cmd ** (_, handler)) => runHandler inputs cmd handler
        Nothing => Nothing
runHandler [] (Parent subCommands) (HParent x) = Nothing

