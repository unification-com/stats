module Main where

import           System.Environment (getArgs)
import           System.Exit        (ExitCode (ExitFailure), exitWith)

import           Radar.Scan         (scan, scanMetrics, scanPorts)
import           Radar.Slack        (postToSlack)
import           Secrets            (getSecret)

version = "1.03"

evaluate scanner = do
  slackChannel <- getSecret "TestAPI"
  case slackChannel of
    Nothing -> print "Slack secret not found"
    Just channel -> do
      ret <- scanner
      case ret of
        Left xs -> do
          postToSlack channel xs
          exitWith (ExitFailure 1)
        Right xs -> print xs

fn :: [Char] -> [String] -> IO ()
fn "all" args = do
  print $ show version
  evaluate scan
  evaluate scanMetrics
  evaluate scanPorts
fn "metrics" args = evaluate scanMetrics
fn "version" args = do
  print $ show version
  return ()
fn _ _ = do
  print "all - HTTP, core metric and port scans"
  print "metrics - only scan core metrirs"
  return ()

main = do
  args <- getArgs
  if length args == 0
    then evaluate scan
    else fn (head args) (tail args)
