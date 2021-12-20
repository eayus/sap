module Sap.Help

import Sap.Definition


rhsHelp : RHS a -> String
rhsHelp (SubCmds xs) =
  """
  The following sub-commands are available:
  \{concatMap (\x => "\t" ++ x.name ++ " - " ++ x.desc ++ "\n") xs}
  """
rhsHelp (Basic params options f) = ""

export
help : Command a -> String
help x =
  """

  \{x.name} - \{x.desc}

  \{rhsHelp x.rhs}
  """


--export
--paramsHelp : List Param -> List Option -> String
