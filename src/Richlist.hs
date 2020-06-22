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
import           Data.Map                        (Map, empty, findWithDefault,
                                                  insertWith, lookup, toList)
import           Data.Time.Clock.POSIX           (posixSecondsToUTCTime)
import           GHC.Generics                    (Generic)
import           Numeric                         (showFFloat)
import           System.IO                       (readFile)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                as H hiding (address, map)
import           Text.Blaze.Html5.Attributes     as A

source = "/home/deploy/extract/genesis.json"

timestamp = "/home/deploy/extract/timestamp"

data Config =
  Config
    { app_state :: AppState
    }
  deriving (Show, Generic)

data AppState =
  AppState
    { auth    :: Auth
    , supply  :: Supply
    , staking :: Staking
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

data Staking =
  Staking
    { delegations :: [Delegation]
    }
  deriving (Show, Generic)

data Coin =
  Coin
    { amount :: Int
    }
  deriving (Show, Generic)

data Delegation =
  Delegation
    { delegator_address :: String
    , shares            :: Double
    , validator_address :: String
    }
  deriving (Show, Generic)

instance FromJSON Config

instance FromJSON AppState

instance FromJSON Staking

instance FromJSON Auth

instance FromJSON Account where
  parseJSON (Object v) = Account <$> v .: "type" <*> v .: "value"

instance FromJSON Delegation where
  parseJSON (Object v) =
    Delegation <$> v .: "delegator_address" <*> (readDouble <$> v .: "shares") <*>
    v .: "validator_address"

instance FromJSON Richlist.Value

instance FromJSON Supply where
  parseJSON (Object v) = Supply <$> v .: "supply"

undConvert :: Integral a => a -> String
undConvert n = showFFloat (Just 2) (fromIntegral n / 1000000000) ""

undConvertD :: RealFloat a => a -> String
undConvertD n = showFFloat (Just 2) (n / 1000000000) ""

percentage :: RealFloat a => a -> String
percentage n = showFFloat (Just 2) n ""

makeURL :: String -> Html
makeURL acc = a ! href (stringValue x) $ (toHtml acc)
  where
    x = "https://explorer.unification.io/account/" ++ acc

readDouble :: String -> Double
readDouble = read

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

renderTable :: [String] -> [[Html]] -> IO String
renderTable headers ds = do
  let tableHead = thead (mapM_ (th . toHtml) headers)
  let rows = mapM_ (\xs -> tr (mapM_ (td . toHtml) xs)) ds
  return $ renderHtml (table ! class_ "statstable" $ tableHead >> rows)

snapshotTime :: IO String
snapshotTime = do
  x <- readFile timestamp
  let xInt = read x :: Int
  return $ show (posixSecondsToUTCTime (fromIntegral xInt))

totalSupply :: IO Int
totalSupply = do
  p <- parse
  case p of
    Nothing -> return defaultSupply
    Just (c) -> return (amount $ Prelude.head $ supply' $ supply $ app_state c)
  where
    defaultSupply = 120799977119380000

userAccounts :: Config -> [(String, Int)]
userAccounts c = zip addxs coinxs
  where
    accxs = accounts . auth . app_state $ c
    standardAccounts =
      filter (\x -> accountType x == "cosmos-sdk/Account") accxs
    nonEmptyAccounts =
      filter (\x -> length (coins (Richlist.value x)) > 0) standardAccounts
    addxs = Richlist.address <$> (Richlist.value <$> nonEmptyAccounts)
    mapper [] = 0
    mapper xs = amount $ Prelude.head xs
    coinxs = map mapper (coins <$> (Richlist.value <$> nonEmptyAccounts))

topFUNDHolders :: IO [(String, Double, Double)]
topFUNDHolders = do
  parsed <- parse
  case parsed of
    Nothing -> return []
    Just p -> do
      let liquidlist = (\(a, b) -> (a, fromIntegral b)) <$> (userAccounts p)
      let stakeMap =
            foldr insertFn empty (delegations . staking . app_state $ p)
      let accountMap = foldr anotherFn stakeMap liquidlist
      let richlist =
            take 100 (reverse (sortBy (compare `on` snd) (toList accountMap)))
      let stakelist = map (\(acc, _) -> findWithDefault 0 acc stakeMap) richlist
      let zipped = map stakePercent (zip richlist stakelist)
      return $ zipped
  where
    insertFn :: Delegation -> Map String Double -> Map String Double
    insertFn (Delegation a s _) mk = insertWith (+) a s mk
    anotherFn :: (String, Double) -> Map String Double -> Map String Double
    anotherFn (a, s) mk = insertWith (+) a s mk
    stakePercent ((a, b), c) = (a, b, c / b * 100)

tableRichlist :: IO String
tableRichlist = do
  xns <- topFUNDHolders
  let headers = ["Account", "Amount in FUND", "Staked %"]
  t <- renderTable headers (map mapper xns)
  return $ t
  where
    mapper (a, b, c) =
      [makeURL a, toHtml (undConvertD b), toHtml (percentage c)]
