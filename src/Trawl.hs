{-# LANGUAGE OverloadedStrings #-}

module Trawl
  ( trawl
  , uploadFile
  ) where

import           Data.Aeson                (encode, object, (.=))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)
import           System.FilePath.Posix     (splitFileName)

import           Parsers.Common            (runPython)

ingestionEnpoint = "https://ingest.unification.io/ingest"

submit ::
     String
  -> String
  -> Maybe String
  -> String
  -> String
  -> String
  -> String
  -> IO Int
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
    resolvedEndpoint =
      case endpoint of
        Nothing -> ingestionEnpoint
        Just x  -> x

ping secret machine endpoint =
  submit secret machine endpoint "string" "ping" "ping" "ping"

diskUsage secret machine endpoint = do
  xs <- runPython "import shutil; d = shutil.disk_usage(\"/\"); print (d.used, d.total)"
  let b = words (xs !! 0)
  submitDiskUsage "Used" (b !! 0)
  submitDiskUsage "1KBlocks" (b !! 1)
  where
    submitDiskUsage = submit secret machine endpoint "integer" "DiskUsage"

uploadFile :: String -> String -> Maybe String -> String -> IO ()
uploadFile secret machine endpoint filename = do
  x <- readFile filename
  submit secret machine endpoint "string" "File" (snd $ splitFileName filename) x
  print "Done submitting"

trawl :: String -> String -> Maybe String -> IO ()
trawl secret machine endpoint = do
  ping secret machine endpoint
  diskUsage secret machine endpoint
  print "Done trawling"
