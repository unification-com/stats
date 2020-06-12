{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Richlist
  (
  ) where

import           Data.Aeson
import           GHC.Generics         (Generic)

import qualified Data.ByteString.Lazy as B

data Config =
  Config
    { app_state :: AppState
    }
  deriving (Show, Generic)

data AppState =
  AppState
    { auth :: Auth
    }
  deriving (Show, Generic)

data Auth =
  Auth
    { accounts :: [Account]
    }
  deriving (Show, Generic)

data Account =
  Account
    { value :: Richlist.Value
    }
  deriving (Show, Generic)

data Value =
  Value
    { address :: String
    , coins   :: [Coin]
    }
  deriving (Show, Generic)

data Coin =
  Coin
    { amount :: Int
    }
  deriving (Show, Generic)

instance FromJSON Config

instance FromJSON AppState

instance FromJSON Auth

instance FromJSON Account

instance FromJSON Richlist.Value

readInt :: String -> Int
readInt = read

instance FromJSON Coin where
  parseJSON (Object v) = Coin <$> (readInt <$> v .: "amount")

source = "/Users/indika/dev/stats/data/genesis-1591864502.json"

jsonFile :: FilePath
jsonFile = source

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

parse :: IO (Maybe Config)
parse = do
  j <- getJSON
  let decoded = decode j
  return $ decoded

-- What am I doing?
test = parse
