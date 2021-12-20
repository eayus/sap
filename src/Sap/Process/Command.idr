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


processCommand' : List Chunk -> Command a -> Error a
processCommand' chunks cmd with (cmd.rhs)
    _ | Basic params options run = do
          let inputs = parseInputs chunks
          args' <- processArgs inputs.args params
          opts' <- processOpts options inputs.opts
          pure $ run args' opts'

    processCommand' [] cmd | SubCmds cmds =
        Left "Expected sub command"

    processCommand' (FlagChunk x :: xs) cmd | SubCmds cmds =
        Left "Unexpected flag \{show x}, expected sub command"

    processCommand' (ArgChunk "help" :: rest) cmd | SubCmds cmds =
        Left $ help cmd

    processCommand' (ArgChunk cmdName :: rest) cmd | SubCmds cmds =
        case find ((cmdName ==) . .name) cmds of
             Just subCmd => processCommand' rest subCmd
             Nothing => Left "Invalid subcommand \{cmdName}"


export
processCommand : List String -> Command a -> Error a
processCommand input cmd = do
    chunks <- bimap show id $ chunkify input
    processCommand' chunks cmd
