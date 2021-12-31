module Sap.Help

import Sap.Definition
import Sap.Parsable
import Data.String


rhsHelp : RHS a -> String
rhsHelp (SubCmds xs) =
  """
  The following sub-commands are available:
  \{concatMap (\x => "\t" ++ x.name ++ " - " ++ x.desc ++ "\n") xs}
  """
rhsHelp (Basic params options _) =
  """
  The following parameters are required:
  \{concatMap (\x => let _ = x.parsable in "(" ++ x.name ++ ":" ++ stringify {a = x.type} ++ ")") params}

  The following options are available:
  \{concatMap (\x => "-" ++ pack [x.short] ++ " | " ++ "--" ++ x.long ++ "\n") options}
  """


help : List String -> Command a -> String
help path x =
  """

  \{unwords $ path ++ [x.name]} - \{x.desc}

  \{rhsHelp x.rhs}
  """


export
printHelp : Help -> IO ()
printHelp (MkHelp _ path x) = putStrLn $ help path x
