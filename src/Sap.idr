module Sap

import public Data.List
import Sap.Help
import System
import public Sap.Util
import public Sap.Definition
import public Sap.Process.Command
import public Sap.Parsable


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
