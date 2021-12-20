module Example

import Sap.Definition
import Sap.Parsable
import Sap.Process.Command
import Sap.Util
import System

%default total


cmd : Command (IO ())
cmd = MkCommand {
    name = "exmaple",
    desc = "Package manager CLI",
    rhs  = SubCmds [
        MkCommand {
            name = "run",
            desc = "Run the built project",
            rhs  = Basic [] [MkOption 'n' "name" ["name" # String]] (\[], [name]
                      => putStrLn "running \{show name}") 
        },
        MkCommand {
            name = "new",
            desc = "Create a new project",
            rhs  = Basic ["filepath" # String] [] (\[filepath], []
                      => putStrLn "new at ...")
        }
    ]
}


partial
main : IO ()
main = do
    _ :: xs <- getArgs

    case processCommand xs cmd of
        Left err => putStrLn err
        Right res => res
