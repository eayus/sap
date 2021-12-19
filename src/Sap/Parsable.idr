module Sap.Parsable

import Sap.Util


public export
interface Parsable (a : Type) where
    parse : String -> Error a


public export
Parsable String where
    parse s = pure s