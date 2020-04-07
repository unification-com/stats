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

data Info =
  Info
    { success :: Bool
    }
  deriving (Eq, Show)

data Ingestion =
  Ingestion
    { machine :: String
    }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Info)

$(deriveJSON defaultOptions ''Ingestion)

type API = "ingest" :> ReqBody '[ JSON] Ingestion :> Post '[ JSON] Info

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = position
  where
    position x = do
      liftIO (print x)
      return (info x)

info :: Ingestion -> Info
info ingestion = Info True

main :: IO ()
main = run 48535 app
