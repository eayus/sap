module Sap

import Sap.Definition
import Sap.Help
import Sap.Process.Command
import System


public export partial
runCommand : Command (IO ()) -> IO ()
runCommand cmd = do
    _ :: xs <- getArgs

    let (res, notes) = runIdentity $ runWriterT $ runEitherT $ processCommand xs cmd

    case res of
         Left (HelpFor x) => printHelp x
         Left (Error x) => putStrLn x
         Right x => do
              putStrLn $ concatMap (++ "\n") notes
              x
