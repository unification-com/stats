module Main where

import           Radar.Scan  (scan)
import           Radar.Slack (postToSlack)
import           Secrets     (getSecret)

main = do
  slackChannel <- getSecret "TestAPI"
  case slackChannel of
    Nothing -> print "Slack secret not found"
    Just channel -> do
      ret <- scan
      case ret of
        Left xs  -> postToSlack channel xs
        Right xs -> print xs
