module Example

import Sap

%default total


cmd : Command (IO ())
cmd = Cmd {
    name = "exmaple",
    desc = "Package manager CLI",
    rhs  = SubCmds [
        Cmd {
            name = "run",
            desc = "Run the built project",
            rhs  = Basic [] [MkOption 'n' "name" ["name" # String]] (\[], [name]
                      => putStrLn "running \{show name}") 
        },
        Cmd {
            name = "new",
            desc = "Create a new project",
            rhs  = Basic ["filepath" # String] [] (\[filepath], []
                      => putStrLn "new at ...")
        }
    ]
}


partial
main : IO ()
main = runCommand cmd
