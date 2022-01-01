module Sap.Process.Command

import Sap.Definition
import Sap.Util
import Sap.Raw.Chunk
import Sap.Raw.Flag
import Sap.Raw.Parse
import Sap.Process.Arg
import Sap.Process.Opt
import Sap.Help
import Data.List


consHelpPath : String -> Result a -> Result a
consHelpPath s = withError go
    where
        go : Failure -> Failure
        go (HelpFor (MkHelp _ cmdPath cmd)) = HelpFor $ MkHelp _ (s :: cmdPath) cmd
        go (Error x) = Error x

toAll : {0 params : List Param} -> Args params r -> All Arg params -> r
toAll fn [] = fn
toAll fn (x :: y) = toAll (fn x) y

processCommand' : List Chunk -> Command a -> Result a
processCommand' chunks cmd with (cmd.rhs)
    _ | Basic params options run = do
          let inputs = parseInputs chunks
          when (any (isHelpFlag . fst) inputs.opts) $ throwError $ HelpFor $ MkHelp _ [] cmd
          args' <- processArgs inputs.args params
          opts' <- processOpts options inputs.opts
          pure $ toAll run args' opts'

    processCommand' [] cmd | SubCmds cmds =
        throwError $ Error "Expected sub command"

    processCommand' (FlagChunk x :: xs) cmd | SubCmds cmds =
        if isHelpFlag x
            then throwError $ HelpFor $ MkHelp _ [] cmd
            else do
                lift $ tell ["Encountered flag before subcommand, ignoring"]
                processCommand' xs cmd

    processCommand' (ArgChunk "help" :: rest) cmd | SubCmds cmds =
        throwError $ HelpFor $ MkHelp _ [] cmd

    processCommand' (ArgChunk cmdName :: rest) cmd | SubCmds cmds =
        case find ((cmdName ==) . .name) cmds of
             Just subCmd => consHelpPath cmd.name $ processCommand' rest subCmd
             Nothing => throwError $ Error "Invalid subcommand \{cmdName}"


export
processCommand : List String -> Command a -> Result a
processCommand input cmd = do
    let chunks = bimap (Error . show) id $ chunkify input
    chunks <- MkEitherT $ pure chunks
    processCommand' chunks cmd
