module NewExample

import Sap.Definition
import Sap.Implementation
import System


cmd : Command (IO ())
cmd = MkCommand {
    name = "exmaple",
    desc = "",
    rhs  = SubComm [
        MkCommand {
            name = "run",
            desc = "",
            rhs  = Params [] [MkOption "--name" Str] (\[], [name] => putStrLn "running \{show name}") --putStrLn "running...")
        },
        MkCommand {
            name = "new",
            desc = "",
            rhs  = Params ["filepath" # Str] [] (\[filepath], [] => putStrLn "new at ...")
        }
    ]
}


partial
main : IO ()
main = do
    _ :: xs <- getArgs

    case handleArgs xs cmd of
        Left err => putStrLn err
        Right res => res