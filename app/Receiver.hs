{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           Sampler                  (injectS)
import           Secrets                  (getSecret)

data Info =
  Info
    { success :: Bool
    }
  deriving (Eq, Show)

data Ingestion =
  Ingestion
    { password :: String
    , machine  :: String
    , datatype :: String
    , metric   :: String
    , key      :: String
    , sample   :: String
    }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Info)

$(deriveJSON defaultOptions ''Ingestion)

type API = "ingest" :> ReqBody '[ JSON] Ingestion :> Post '[ JSON] Info

app :: String -> Application
app secret = serve api (server secret)

api :: Proxy API
api = Proxy

server :: String -> Server API
server secret = position
  where
    position x = do
      case (password x == secret) of
        True -> do
          liftIO (injectS (Just $ machine x) (metric x) (key x) (sample x))
          return (Info True)
        False -> return (Info False)

info :: Ingestion -> Info
info ingestion = Info True

main :: IO ()
main = do
  secret <- getSecret "ReceiverAccess"
  case secret of
    Nothing -> print "ReceiverAccess secret not found"
    Just pass -> do
      run 48535 (app pass)
