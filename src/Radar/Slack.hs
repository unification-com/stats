{-# LANGUAGE OverloadedStrings #-}

module Radar.Slack
  ( postToSlack
  ) where

import           Data.Aeson                (encode, object, (.=))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)

postToSlack :: String -> String -> IO ()
postToSlack channel msg = do
  manager <- newManager tlsManagerSettings
  let requestObject = object ["text" .= (msg :: String)]
  initialRequest <-
    parseRequest ("https://hooks.slack.com/services/" ++ channel)
  let request =
        initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode requestObject
          , requestHeaders =
              [("Content-Type", "application/json; charset=utf-8")]
          }
  response <- httpLbs request manager
  putStrLn $ "Posted to slack: " ++ show (msg)
  putStrLn $
    "The status code was: " ++ show (statusCode $ responseStatus response)
  return ()
