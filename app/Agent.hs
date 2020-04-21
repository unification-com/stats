module Main where

import           System.Environment (getArgs)

import           Secrets            (getMachine, getSecret)
import           Trawl              (trawl, uploadFile)

main = do
  args <- getArgs
  secret <- getSecret "ReceiverAccess"
  case secret of
    Nothing -> print "ReceiverAccess secret not found"
    Just pass -> do
      machine <- getMachine
      case machine of
        Nothing -> print "Machine name not found"
        Just m -> do
          endpoint <- getSecret "IngestionEndpoint"
          if length args == 0
            then do
              print $
                "Collecting stats for " ++
                m ++ " with endpoint " ++ show endpoint
              trawl pass m endpoint
            else do
              case head args of
                "upload" -> do
                  let filename = head (tail args)
                  print $
                    "Uploading file from " ++
                    m ++ " with endpoint " ++ show endpoint
                  uploadFile pass m endpoint filename
