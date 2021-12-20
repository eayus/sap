module Sap.Help

import Sap.Definition


rhsHelp : RHS a -> String
rhsHelp (SubComm xs) =
  """
  The following sub-commands are available:
  \{concatMap (\x => "\t" ++ x.name ++ "\n") xs}
  """
rhsHelp (Params params options f) = ""

export

help : Command a -> String
help x =
  """
  \{x.name} - \{x.desc}

  \{rhsHelp x.rhs}
  """
