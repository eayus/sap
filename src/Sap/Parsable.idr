module Sap.Parsable

import Sap.Util


public export
interface Parsable (a : Type) where
    parse : String -> Error a


export
parseTo : String -> (a : Type) -> {auto _ : Parsable a} -> Error a 
parseTo s _ = parse s


public export
Parsable String where
    parse s = pure s
