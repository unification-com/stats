{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Richlist
  ( tableRichlist
  , snapshotTime
  )
where

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           Data.Function                  ( on )
import           Data.List                      ( sortBy )
import           Data.Map                       ( Map
                                                , empty
                                                , findWithDefault
                                                , insertWith
                                                , lookup
                                                , toList
                                                , unionWith
                                                )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           GHC.Generics                   ( Generic )
import           Numeric                        ( showFFloat )
import           Renderer                       ( makeURL
                                                , percentage
                                                , renderTable
                                                , undCommaSeperate
                                                )
import           System.IO                      ( readFile )

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
    , distribution :: Distribution
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

data Distribution =
  Distribution
    { delegator_starting_infos :: [DelegationStartingInfo]
    }
  deriving (Show, Generic)

data DelegationStartingInfo =
  DelegationStartingInfo
    { delegator_address_starting :: String
    , starting_info :: StartingInfo
    }
  deriving (Show, Generic)

data StartingInfo =
  StartingInfo
    { stake :: Double
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
    { delegations   :: [Delegation]
    , redelegations :: [Redelegation]
    , unbonding_delegations :: [UnbondingDelegation]
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

data Redelegation =
  Redelegation
    { redelegator_address :: String
    , entries           :: [RedelegationEntry]
    }
  deriving (Show, Generic)

data UnbondingDelegation =
  UnbondingDelegation
    { undelegator_address :: String
    , unentries           :: [UnbondingDelegationEntry]
    }
  deriving (Show, Generic)

data RedelegationEntry =
  RedelegationEntry
    { shares_dst        :: Double
    }
  deriving (Show, Generic)

data UnbondingDelegationEntry =
  UnbondingDelegationEntry
    { balance        :: Double
    }
  deriving (Show, Generic)

instance FromJSON AppState

instance FromJSON Config

instance FromJSON DelegationStartingInfo where
  parseJSON (Object v) = DelegationStartingInfo <$> v .: "delegator_address" <*> v .: "starting_info"

instance FromJSON Distribution

instance FromJSON StartingInfo where
  parseJSON (Object v) = StartingInfo <$> (readDouble <$> v .: "stake")

instance FromJSON Staking

instance FromJSON Auth

instance FromJSON Account where
  parseJSON (Object v) = Account <$> v .: "type" <*> v .: "value"

instance FromJSON Delegation where
  parseJSON (Object v) =
    Delegation <$> v .: "delegator_address" <*> (readDouble <$> v .: "shares") <*>
    v .: "validator_address"

instance FromJSON Redelegation where
  parseJSON (Object v) = Redelegation <$> v .: "delegator_address" <*> v .: "entries"

instance FromJSON RedelegationEntry where
  parseJSON (Object v) = RedelegationEntry <$> (readDouble <$> v .: "shares_dst")

instance FromJSON UnbondingDelegation where
  parseJSON (Object v) = UnbondingDelegation <$> v .: "delegator_address" <*> v .: "entries"

instance FromJSON UnbondingDelegationEntry where
  parseJSON (Object v) = UnbondingDelegationEntry <$> (readDouble <$> v .: "balance")

instance FromJSON Richlist.Value

instance FromJSON Supply where
  parseJSON (Object v) = Supply <$> v .: "supply"

undConvert :: Integral a => a -> String
undConvert n = showFFloat (Just 2) (fromIntegral n / 1000000000) ""

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

snapshotTime :: IO String
snapshotTime = do
  x <- readFile timestamp
  let xInt = read x :: Int
  return $ show (posixSecondsToUTCTime (fromIntegral xInt))

userAccounts :: Config -> [(String, Int)]
userAccounts c = zip addxs coinxs
 where
  accxs            = accounts . auth . app_state $ c
  standardAccounts = filter (\x -> accountType x == "cosmos-sdk/Account") accxs
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
    Just p  -> do
      let liquidlist = (\(a, b) -> (a, fromIntegral b)) <$> (userAccounts p)
      let startingMap = foldr
            insertDSI
            empty
            (delegator_starting_infos . distribution . app_state $ p)
      let redelegs = redelegations . staking . app_state $ p
      let redelegationMap =
            foldr insertTupleFn empty (map redelegationCollapser redelegs)
      let unbonds = unbonding_delegations . staking . app_state $ p
      let unbondingMap =
            foldr insertTupleFn empty (map unbondingCollapser unbonds)
      let stakeMap =
            foldr insertFn empty (delegations . staking . app_state $ p)
      let united     = unionWith (+) startingMap unbondingMap
      let accountMap = foldr anotherFn united liquidlist
      let richlist =
            take 100 (reverse (sortBy (compare `on` snd) (toList accountMap)))
      let stakelist =
            map (\(acc, _) -> findWithDefault 0 acc startingMap) richlist
      let zipped = map stakePercent (zip richlist stakelist)
      return $ zipped
 where
  insertFn :: Delegation -> Map String Double -> Map String Double
  insertFn (Delegation a s _) mk = insertWith (+) a s mk
  insertDSI :: DelegationStartingInfo -> Map String Double -> Map String Double
  insertDSI (DelegationStartingInfo a (StartingInfo s)) mk =
    insertWith (+) a s mk
  insertTupleFn :: (String, Double) -> Map String Double -> Map String Double
  insertTupleFn (a, v) mk = insertWith (+) a v mk
  anotherFn :: (String, Double) -> Map String Double -> Map String Double
  anotherFn (a, s) mk = insertWith (+) a s mk
  stakePercent ((a, b), c) = (a, b, c / b * 100)
  redelegationCollapser :: Redelegation -> (String, Double)
  redelegationCollapser (Redelegation a e) =
    (a, foldr (\x b -> shares_dst x + b) 0 e)
  unbondingCollapser :: UnbondingDelegation -> (String, Double)
  unbondingCollapser (UnbondingDelegation a e) =
    (a, foldr (\x b -> balance x + b) 0 e)

tableRichlist :: IO String
tableRichlist = do
  xns <- topFUNDHolders
  let headers = ["Account", "Amount in FUND", "Staked %"]
  return $ renderTable headers (map mapper xns)
  where mapper (a, b, c) = [makeURL a, undCommaSeperate b, percentage c]
