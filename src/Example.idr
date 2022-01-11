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
            rhs  = Basic [] [simpleOpt "name" String]
                (\[], opts =>
                    case optByLong "name" opts of
                         Just [name] => putStrLn "Running \{name}!"
                         Nothing     => putStrLn "Running!")
        },
        Cmd {
            name = "new",
            desc = "Create a new project",
            rhs  = Basic ["filepath" # String] [] (\[filepath], []
                      => putStrLn "new at \{show filepath}")
        }
    ]
}


partial
main : IO ()
main = runCommand cmd
