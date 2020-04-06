{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Stat = Stat
  { statId        :: Int
  , statValue     :: Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Stat)

type API = "stats" :> Get '[JSON] [Stat]

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return stats

stats :: [Stat]
stats = [ Stat 1 6
        , Stat 2 5
        ]

main :: IO ()
main = run 8080 app
