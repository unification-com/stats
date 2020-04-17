module Main where

import           System.Environment (getArgs)

import           Radar.Scan         (scan, scanPorts)
import           Radar.Slack        (postToSlack)
import           Secrets            (getSecret)

fn :: [Char] -> [String] -> IO ()
fn "internal" args = do
  slackChannel <- getSecret "TestAPI"
  case slackChannel of
    Nothing -> print "Slack secret not found"
    Just channel -> do
      ret <- scanPorts
      case ret of
        Left xs  -> postToSlack channel xs
        Right xs -> print xs
fn _ _ = do
  print "internal"
  return $ ()

compute = do
  slackChannel <- getSecret "TestAPI"
  case slackChannel of
    Nothing -> print "Slack secret not found"
    Just channel -> do
      ret <- scan
      case ret of
        Left xs  -> postToSlack channel xs
        Right xs -> print xs

main = do
  args <- getArgs
  if length args == 0
    then compute
    else fn (head args) (tail args)
