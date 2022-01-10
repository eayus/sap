module Sap.Help

import Sap.Definition
import Sap.Parsable
import Data.String
import Control.ANSI


subCommandHelp : Command a -> String
subCommandHelp cmd = "\t\{show $ bolden cmd.name} - \{cmd.desc}"


paramHelp : Param -> String
paramHelp p = let _ = p.parsable in show $ italicize $ "(\{p.name} : \{stringify {a = p.type}})"


optionHelp : Option -> String
optionHelp option = "\t\{short}, \{long}   \{unwords $ map paramHelp option.params}"
    where
        short : String
        short = "-\{pack [option.short]}"

        long : String
        long = "--\{option.long}"


rhsHelp : RHS a -> String
rhsHelp (SubCmds xs) =
  """
  The following sub-commands are available:
  \{unlines $ map subCommandHelp xs}
  """
rhsHelp (Basic params options _) =
  """
  The following parameters are required:
  \{unlines $ map (("\t" ++) . paramHelp) params}

  The following options are available:
  \{unlines $ map optionHelp options}
  """


help : List String -> Command a -> String
help path x =
  """
  \{show $ bolden $ unwords $ path ++ [x.name]} - \{x.desc}
  \n\{rhsHelp x.rhs}
  """


export
printHelp : Help -> IO ()
printHelp (MkHelp _ path x) = putStrLn $ help path x
