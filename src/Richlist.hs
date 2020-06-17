{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Richlist
  ( tableRichlist
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy            as B
import           Data.Function                   (on)
import           Data.List                       (sortBy)
import           GHC.Generics                    (Generic)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                as H hiding (address, map)
import           Text.Blaze.Html5.Attributes     as A

source = "/home/deploy/extract/genesis.json"

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

jsonFile :: FilePath
jsonFile = Richlist.source

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

parse :: IO (Maybe Config)
parse = do
  j <- getJSON
  let decoded = decode j
  return $ decoded

--TODO: A sanity check
isolate :: Maybe Config -> Maybe [(String, Int)]
isolate Nothing = Nothing
isolate (Just c) = Just ret
  where
    accxs = accounts (auth (app_state c))
    addxs = address <$> (Richlist.value <$> accxs)
    mapper [] = 0
    mapper xs = amount $ Prelude.head xs
    coinxs = map mapper (coins <$> (Richlist.value <$> accxs))
    ret = zip addxs coinxs

richlist = do
  p <- parse
  let iso = isolate p
  case iso of
    Nothing -> return $ Nothing
    Just (xs) -> do
      let zs = reverse (sortBy (compare `on` (\(a, b) -> b)) xs)
      return $ Just (take 100 zs)

renderTable :: [String] -> [[String]] -> IO String
renderTable headers ds = do
  let tableHead = thead (mapM_ (th . toHtml) headers)
  let rows = mapM_ (\xs -> tr (mapM_ (td . toHtml) xs)) ds
  return $ renderHtml (table ! class_ "statstable" $ tableHead >> rows)

tableRichlist :: IO String
tableRichlist = do
  xns <- richlist
  let headers = ["Account Number", "Amount"]
  case xns of
    Nothing -> do
      t <- renderTable headers []
      return $ t
    Just (xs) -> do
      t <- renderTable headers (mapper <$> xs)
      return $ t
  where
    mapper (a, b) = [a, show b]

test = tableRichlist
