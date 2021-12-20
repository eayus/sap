module Example

import Sap.Definition
import Sap.Parsable
import Sap.Process.Command
import Sap.Util
import System


cmd : Command (IO ())
cmd = MkCommand {
    name = "exmaple",
    desc = "",
    rhs  = SubComm [
        MkCommand {
            name = "run",
            desc = "",
            rhs  = Params [] [MkOption 'n' "name" ["name" # String]] (\[], [name] => putStrLn "running \{show name}") --putStrLn "running...")
        },
        MkCommand {
            name = "new",
            desc = "",
            rhs  = Params ["filepath" # String] [] (\[filepath], [] => putStrLn "new at ...")
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
