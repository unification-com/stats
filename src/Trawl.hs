{-# LANGUAGE OverloadedStrings #-}

module Trawl
  ( trawl
  , uploadFile
  ) where

import           Data.Aeson                (encode, object, (.=))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)

import           Parsers.Common            (saltjq)

ingestionEnpoint = "https://ingest-testnet.unification.io/ingest"

submit :: String -> String -> Maybe String -> String -> String -> String -> String -> IO Int
submit secret machine endpoint datatype metric key sample = do
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
  initialRequest <- parseRequest resolvedEndpoint
  let request =
        initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode requestObject
          , requestHeaders =
              [("Content-Type", "application/json; charset=utf-8")]
          }
  response <- httpLbs request manager
  let code = statusCode $ responseStatus response
  if code /= 200
    then putStrLn $ "Error: request with code: " ++ show code
    else putStrLn $ "200 OK"
  return code
  where
    resolvedEndpoint = case endpoint of
      Nothing -> ingestionEnpoint
      Just x -> x

ping secret machine endpoint = submit secret machine endpoint "string" "ping" "ping" "ping"

diskUsage secret machine endpoint = do
  xs <- saltjq "disk.usage" ["/", "1K-blocks"] ".local[$a] | .used,.[$b]"
  submitDiskUsage "Used" (xs !! 0)
  submitDiskUsage "1KBlocks" (xs !! 1)
  where
    submitDiskUsage = submit secret machine endpoint "integer" "DiskUsage"

uploadFile :: String -> String -> Maybe String -> String -> IO ()
uploadFile secret machine endpoint filename = do
  x <- readFile filename
  submit secret machine endpoint "string" "File" "Uncompressed" x
  print "Done submitting"

trawl :: String -> String -> Maybe String -> IO ()
trawl secret machine endpoint = do
  ping secret machine endpoint
  diskUsage secret machine endpoint
  print "Done trawling"
