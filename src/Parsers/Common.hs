module Parsers.Common
  ( restEndpoint
  , curljq
  , saltjq
  ) where

import           Data.List      (intercalate)
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

saltjq :: String -> [String] -> String -> IO [String]
saltjq grain args query = do
  print $ shell_cmd
  (_, Just hOut, _, hProc) <-
    createProcess ((shell (shell_cmd)) {std_out = CreatePipe})
  exitCode <- waitForProcess hProc
  output <- hGetContents hOut
  return $ removePunc <$> lines output
  where
    shell_cmd = saltCmd grain args query

removePunc :: String -> String
removePunc xs = filter (not . (`elem` exclusions)) xs
  where
    exclusions = ",.?!-:;\"\'\\" :: String

saltCmd grain args query =
  saltSection grain ++ " | jq " ++ argStr ++ " '" ++ query ++ "' "
  where
    saltSection grain =
      "salt-call --local " ++ grain ++ " --output json --log-level error"
    argStr = intercalate " " $ jqArgs args
    jqArgs args =
      (\(x, y) -> "--arg " ++ [y] ++ " '" ++ x ++ "'") <$> (zip args vars)
    vars = take (length args) ['a' ..]
