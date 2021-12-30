module Sap.Process.Opt

import Data.List
import Sap.Definition
import Sap.Raw.Flag
import Sap.Raw.Parse
import Sap.Util
import Sap.Process.Arg


lookupOpt : Option -> OptMap -> Maybe (List String)
lookupOpt opt optMap = lookup (Short opt.short) optMap
                    <|> lookup (Long opt.long) optMap


-- TODO: iterate OptMap instead so we can identify invalid opts
--       and more easily detect help opts.
export
processOpts : (opts : List Option) -> OptMap
           -> Result (All (Maybe . All Arg . .params) opts)
processOpts []        optMap = pure []
processOpts (opt :: opts) optMap =
    case lookupOpt opt optMap of
         Just args => do
            args' <- processArgs args opt.params
            zs <- processOpts opts optMap
            pure $ Just args' :: zs
         Nothing => (Nothing ::) <$> processOpts opts optMap
