module Sap.Process.Arg

import Sap.Util
import Sap.Definition
import Sap.Parsable


processArg : String -> (param : Param) -> Result (Arg param)
processArg s param = let _ = param.parsable
                         res = parseTo s param.type
                         res' = bimap Error id res
                     in MkEitherT $ pure res'


export
processArgs : List String -> (params : List Param) -> Result (All Arg params)
processArgs [] [] = pure []
processArgs [] (x :: xs) = throwError $ Error "Expected another parameter"
processArgs (x :: xs) [] = throwError $ Error "Unexpected parameter"
processArgs (x :: xs) (y :: ys) = [| processArg x y :: processArgs xs ys |]
