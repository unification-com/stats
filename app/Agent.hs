module Main where

import           Secrets (getMachine, getSecret)
import           Trawl   (trawl)

main = do
  secret <- getSecret "ReceiverAccess"
  case secret of
    Nothing -> print "ReceiverAccess secret not found"
    Just pass -> do
      machine <- getMachine
      case machine of
        Nothing -> print "Machine name not found"
        Just m -> do
          print $ "Collecting stats for " ++ m
          trawl pass m
