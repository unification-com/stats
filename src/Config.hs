module Config
  ( accounts
  , connectionString
  , coreMetricsPath
  ) where

import           Data.List          (elemIndex)
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           System.Environment (getEnvironment)

accounts =
  [ "und1nkh2dteta8drxntqp646sr6vz74lt9w9yc60pd"
  , "und1hu50gv6a55tz592tk4y7hstmxfw9y9sg4kz2wm"
  , "und19enk9tmm98sa6yzk4pwarxamkkc0nth923e8nz" -- UNDEurope
  , "und1z575nxpemg4dtcrv0rd2u5pwctjxjzsy2hyzjn"
  , "und1w2dlf0793gk3m5zk8e554stg97x7uw95xl27kw" -- Shark
  , "und10ta4pvss0wg6t9yhq7k6y7lhk7v95uqg7p22nu"
  , "und1m4y8cxqhvmqltwyehtdyk3ymgwd854j7wmvc5k"
  , "und15s4ec3s97tu4pstk8tq86l5ues4dxnmadqmrjl"
  , "und1q43fg7x7yn6wv3zxwxjknj7xv7tfqd0ahnc0mv"
  , "und16twxa6lyj7uhp56tukrcfz2p6q93mrxgqvrspk" -- SerenityTN
  , "und1mtxp3jh5ytygjfpfyx4ell495zc2m4k8ft8uly" -- tokenswap account
  , "und12zns8tfm0g2rskl4f9zg2hr9n53agkyvtftngs" -- Finchain
  , "und10xpk56etf9s7efezvlu3qu5rg9djfm979cu4ax" -- MediaRallyAWS2020
  , "und12t657d878j97c3qn55r4zwx3smmwd3rxenc6k6" -- BidiPass
  , "und1duyhqzcgrzjy9y2yvueur2h3e2yqxhjl3jq2zl" -- Yellow.com
  , "und1jvxdh905vpcnzarwqt9pqdjq8ks8jdh7fm006l" -- COACHK
  , "und17jv7rerc2e3undqumpf32a3xs9jc0kjk4z2car" -- Faucet
  ]

has vars = case hasCell of
  Nothing -> Nothing
  (Just x) -> snd <$> Just (vars !! x)
  where
    hasCell = elemIndex "bits_env" (fst <$> vars)

connectionString = do
  vars <- getEnvironment
  case has vars of
    Nothing -> return (encode "postgresql://postgres:password@localhost:8432/postgres")
    (Just x) -> case x of
      "warp" -> return (encode "postgresql://indika:password@localhost:5432/warp")
      "aws" -> return (encode "postgresql://postgres:password@localhost:5432/postgres")
  where
    encode = encodeUtf8 . T.pack


coreMetricsPath = do
  vars <- getEnvironment
  case has vars of
    Nothing -> return "/tmp"
    (Just x) -> case x of
      "warp" -> return "/tmp"
      "aws" -> return "/home/deploy/src/coremetrics"
