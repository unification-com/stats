module Parsers.Common
  ( restEndpoint
  , curljq
  ) where

import           System.IO      (hGetContents)
import           System.Process

restEndpoint :: String -> String
restEndpoint segment = "https://rest-testnet.unification.io/" ++ segment

curljq :: String -> String -> IO [String]
curljq url query = do
  (_, Just hOut, _, hProc) <-
    createProcess ((shell (shell_cmd)) {std_out = CreatePipe})
  exitCode <- waitForProcess hProc
  output <- hGetContents hOut
  return $ lines output
  where
    shell_cmd = "curl -s " ++ url ++ " | jq -r '" ++ query ++ "'"
