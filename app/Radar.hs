module Main where

import           System.Environment (getArgs)

import           Radar.Scan         (scan, scanPorts)
import           Radar.Slack        (postToSlack)
import           Secrets            (getSecret)

evaluate scanner = do
  slackChannel <- getSecret "TestAPI"
  case slackChannel of
    Nothing -> print "Slack secret not found"
    Just channel -> do
      ret <- scanner
      case ret of
        Left xs  -> postToSlack channel xs
        Right xs -> print xs

fn :: [Char] -> [String] -> IO ()
fn "internal" args = evaluate scanPorts
fn _ _ = do
  print "internal"
  return $ ()

main = do
  args <- getArgs
  if length args == 0
    then evaluate scan
    else fn (head args) (tail args)
