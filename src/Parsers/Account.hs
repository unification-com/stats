module Parsers.Account
  ( queryMainchainAccount
  , supply
  ) where

import           System.IO      (hGetContents)
import           System.Process

restEndpoint :: String -> String
restEndpoint segment = "https://rest-testnet.unification.io/" ++ segment

curljq :: String -> String -> IO String
curljq url query = do
  (_, Just hOut, _, hProc) <-
    createProcess ((shell (shell_cmd)) {std_out = CreatePipe})
  exitCode <- waitForProcess hProc
  output <- hGetContents hOut
  return $ head (lines output)
  where
    shell_cmd = "curl -s " ++ url ++ " | jq -r '" ++ query ++ "'"

queryMainchainAccount account = do
  val <-
    curljq
      (restEndpoint "auth/accounts/" ++ account)
      ".result.account.value.coins[0].amount"
  if val == "null"
    then return "0"
    else return val

-- TODO: This could be optimized
supply = do
  amount <- curljq supplyEndpoint ".result.amount"
  locked <- curljq supplyEndpoint ".result.locked"
  total <- curljq supplyEndpoint ".result.total"
  return $ (amount, locked, total)
  where
    supplyEndpoint = restEndpoint "supply/total"
