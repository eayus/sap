module Example

import Sap
import System


MainCommand : Command
MainCommand = Parent [
    ("new", Child [("filepath", Str)]),
    ("run", Child []),
    ("build", Child [])
]


argHandler : Handler MainCommand (IO ())
argHandler = HParent [
    (Name "new",   HChild (\[(name {- Name "filepath" -}, filepath)]
        => putStrLn "new project at \{filepath}")),

    (Name "run",   HChild (\[]
        => putStrLn "running")),

    (Name "build", HChild (\[]
        => putStrLn "building"))
]


partial
main : IO ()
main = do
    _ :: inputs <- getArgs

    case runHandler inputs MainCommand argHandler of
        Just x  => x
        Nothing => putStrLn "Invalid args"