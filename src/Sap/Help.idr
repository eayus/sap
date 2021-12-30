module Sap.Help

import Sap.Definition
import Data.String


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


export
printHelp : Help -> IO ()
printHelp (MkHelp _ path x) =
    case x of
            (Cmd name desc (SubCmds xs)) => putStrLn "Help for \{unwords (path ++ [name])}"
            (Cmd name desc (Basic params options f)) => putStrLn "Help for \{unwords (path ++ [name])}"
