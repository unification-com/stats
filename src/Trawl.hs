{-# LANGUAGE OverloadedStrings #-}

module Trawl
  ( trawl
  ) where

import           Data.Aeson                (encode, object, (.=))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)

ingestionEnpoint = "https://ingest-testnet.unification.io/ingest"

submit :: String -> String -> String -> String -> String -> String -> IO ()
submit secret machine datatype metric key sample = do
  manager <- newManager tlsManagerSettings
  let requestObject =
        object
          [ "password" .= (secret :: String)
          , "machine" .= (machine :: String)
          , "datatype" .= (datatype :: String)
          , "metric" .= (metric :: String)
          , "key" .= (key :: String)
          , "sample" .= (sample :: String)
          ]
  initialRequest <- parseRequest ingestionEnpoint
  let request =
        initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode requestObject
          , requestHeaders =
              [("Content-Type", "application/json; charset=utf-8")]
          }
  response <- httpLbs request manager
  putStrLn $
    "The status code was: " ++ show (statusCode $ responseStatus response)
  return ()

ping secret machine = submit secret machine "string" "ping" "ping" "ping"

trawl :: String -> String -> IO ()
trawl secret machine = do
  ping secret machine
  print "Done trawling"
