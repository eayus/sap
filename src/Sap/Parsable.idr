module Sap.Parsable

import Sap.Util


-- The "parse" function should probably explictly take 'a'

public export
interface Parsable (a : Type) where
    parse : String -> Error a


public export
Parsable String where
    parse s = pure s
