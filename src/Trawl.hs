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

submit :: String -> String -> String -> String -> String -> String -> IO Int
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
  let code = statusCode $ responseStatus response
  if code /= 200 then
    putStrLn $ "Error: request with code: " ++ show code
  else
    putStrLn $ "200 OK"
  return code

ping secret machine = submit secret machine "string" "ping" "ping" "ping"

removePunc :: String -> String
removePunc xs = filter (not . (`elem` exclusions)) xs
  where
    exclusions = ",.?!-:;\"\'\\" :: String

diskUsage secret machine = do
  out <- saltjq "disk.usage" ["/", "1K-blocks"] ".local[$a] | .used,.[$b]"
  let xs = removePunc <$> out
  submit secret machine "integer" "DiskUsage" "Used" (xs !! 0)
  submit secret machine "integer" "DiskUsage" "1KBlocks" (xs !! 1)

trawl :: String -> String -> IO ()
trawl secret machine = do
  ping secret machine
  diskUsage secret machine
  print "Done trawling"
