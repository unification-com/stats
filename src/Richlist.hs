{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Richlist
  ( tableRichlist
  , snapshotTime
  , totalSupply
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy            as B
import           Data.Function                   (on)
import           Data.List                       (sortBy)
import           GHC.Generics                    (Generic)
import           Numeric                         (showFFloat)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                as H hiding (address, map)
import           Text.Blaze.Html5.Attributes     as A

source = "/home/deploy/extract/genesis.json"

data Config =
  Config
    { app_state    :: AppState
    , genesis_time :: String
    }
  deriving (Show, Generic)

data AppState =
  AppState
    { auth   :: Auth
    , supply :: Supply
    }
  deriving (Show, Generic)

data Auth =
  Auth
    { accounts :: [Account]
    }
  deriving (Show, Generic)

data Account =
  Account
    { accountType :: String
    , value       :: Richlist.Value
    }
  deriving (Show, Generic)

data Value =
  Value
    { address :: String
    , coins   :: [Coin]
    }
  deriving (Show, Generic)

data Supply =
  Supply
    { supply' :: [Coin]
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

instance FromJSON Account where
  parseJSON (Object v) = Account <$> v .: "type" <*> v .: "value"

instance FromJSON Richlist.Value

instance FromJSON Supply where
  parseJSON (Object v) = Supply <$> v .: "supply"

undConvert :: Integral a => a -> String
undConvert n = showFFloat (Just 2) (fromIntegral n / 1000000000) ""

makeURL :: String -> Html
makeURL acc = a ! href (stringValue x) $ (toHtml acc)
  where
    x = "https://explorer.unification.io/account/" ++ acc

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
    standardAccounts =
      filter (\x -> accountType x == "cosmos-sdk/Account") accxs
    nonEmptyAccounts =
      filter (\x -> length (coins (Richlist.value x)) > 0) standardAccounts
    addxs = address <$> (Richlist.value <$> nonEmptyAccounts)
    mapper [] = 0
    mapper xs = amount $ Prelude.head xs
    coinxs = map mapper (coins <$> (Richlist.value <$> nonEmptyAccounts))
    ret = zip addxs coinxs

richlist = do
  p <- parse
  let iso = isolate p
  case iso of
    Nothing -> return $ Nothing
    Just (xs) -> do
      let zs = reverse (sortBy (compare `on` (\(a, b) -> b)) xs)
      return $ Just (take 100 zs)

renderTable :: [String] -> [[Html]] -> IO String
renderTable headers ds = do
  let tableHead = thead (mapM_ (th . toHtml) headers)
  let rows = mapM_ (\xs -> tr (mapM_ (td . toHtml) xs)) ds
  return $ renderHtml (table ! class_ "statstable" $ tableHead >> rows)

tableRichlist :: IO String
tableRichlist = do
  xns <- richlist
  let headers = ["Account", "Amount in FUND"]
  case xns of
    Nothing -> do
      t <- renderTable headers []
      return $ t
    Just (xs) -> do
      t <- renderTable headers (map mapper xs)
      return $ t
  where
    mapper (a, b) = [makeURL a, toHtml (undConvert b)]

snapshotTime :: IO String
snapshotTime = do
  p <- parse
  case p of
    Nothing  -> return "Error parsing data"
    Just (x) -> return (genesis_time x)

totalSupply :: IO Int
totalSupply = do
  p <- parse
  case p of
    Nothing -> return defaultSupply
    Just (c) -> return (amount $ Prelude.head $ supply' $ supply $ app_state c)
  where
    defaultSupply = 120799977119380000

test = totalSupply
