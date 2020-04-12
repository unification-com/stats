{-# LANGUAGE OverloadedStrings #-}

module Trawl
  ( trawl
  ) where

import           Data.Aeson                (encode, object, (.=))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)

import           Parsers.Common            (saltjq)

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

diskUsage secret machine = do
  out <- saltjq "disk.usage" ["/", "1K-blocks"] ".local[$a] | .used,.[$b]"
  print $ show out
  submit secret machine "integer" "DiskUsage" "Used" (out !! 0)
  submit secret machine "integer" "DiskUsage" "1KBlocks" (out !! 1)

trawl :: String -> String -> IO ()
trawl secret machine = do
  ping secret machine
  diskUsage secret machine
  print "Done trawling"
