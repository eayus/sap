module Sap.Process.Arg

import Sap.Util
import Sap.Definition
import Sap.Parsable


processArg : String -> (param : Param) -> Error (Arg param)
processArg s param = let _ = param.parsable in parseTo s param.type


export
processArgs : List String -> (params : List Param) -> Error (All Arg params)
processArgs [] [] = pure []
processArgs [] (x :: xs) = Left "Expected another parameter"
processArgs (x :: xs) [] = Left "Unexpected parameter"
processArgs (x :: xs) (y :: ys) = [| processArg x y :: processArgs xs ys |]
