{-# LANGUAGE OverloadedStrings #-}

module Report
  ( tableTotalSupply24H
  , tableAccounts24H
  , tableValidators24H
  ) where

import           Control.Monad                   (forM_)
import           Data.Text                       as T
import           Data.Time.Clock                 (UTCTime, addUTCTime,
                                                  getCurrentTime)
import           Database.PostgreSQL.Simple

import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                as H
import           Text.Blaze.Html5.Attributes     as A

import           Config                          (accounts, connectionString)
import           Parsers.Validator               (Validator (..), readValidator)


obtainSample :: Connection -> String -> String -> (UTCTime, UTCTime) -> IO Int
obtainSample c metric feature (a, b) = do
  a <-
    query
      c
      "SELECT a.sample - b.sample FROM \
      \(SELECT sample FROM stats.metrics WHERE metric = ? and feature = ? AND \
      \utc_date > ? AND utc_date < ? ORDER BY utc_date DESC LIMIT 1) a CROSS JOIN \
      \(SELECT sample FROM stats.metrics WHERE metric = ? and feature = ? AND utc_date > ? AND utc_date < ? \
      \ORDER BY utc_date ASC LIMIT 1) b" $
    [metric, feature, show a, show b, metric, feature, show a, show b] :: IO [Only Int]
  return $ fromOnly $ Prelude.head a

obtainSampleF :: Connection -> String -> String -> (UTCTime, UTCTime) -> IO Double
obtainSampleF c metric feature (a, b) = do
  a <-
    query
      c
      "SELECT a.sample - b.sample FROM \
      \(SELECT sample FROM stats.metricsf WHERE metric = ? and feature = ? AND \
      \utc_date > ? AND utc_date < ? ORDER BY utc_date DESC LIMIT 1) a CROSS JOIN \
      \(SELECT sample FROM stats.metricsf WHERE metric = ? and feature = ? AND utc_date > ? AND utc_date < ? \
      \ORDER BY utc_date ASC LIMIT 1) b" $
    [metric, feature, show a, show b, metric, feature, show a, show b] :: IO [Only Double]
  return $ fromOnly $ Prelude.head a



validators :: Connection -> (UTCTime, UTCTime) -> IO [String]
validators c (a, b) = do
  a <-
    query
      c
      "SELECT DISTINCT operator_address from stats.validators WHERE utc_date > ? AND utc_date < ?;" $
    [show a, show b] :: IO [Only String]
  return $ fromOnly <$> a

window :: IO (UTCTime, UTCTime)
window = do
  now <- getCurrentTime
  return $ (addUTCTime (-3600 * 24) now, now)

truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral (floor (x * t))) / t
  where
    t = 10 ^ n

undConvert :: Integral a => a -> Double
undConvert n = truncate' (fromIntegral n / 1000000000) 2

makeURL :: String -> Html
makeURL acc = a ! href (stringValue x) $ (toHtml acc)
  where
    x = "https://explorer-testnet.unification.io/account/" ++ acc

makeValidatorURL :: String -> Text -> Html
makeValidatorURL acc m = a ! href (textValue x) $ (toHtml m)
  where
    x =
      T.concat
        [ T.pack "https://explorer-testnet.unification.io/validator/"
        , T.pack acc
        ]

metricDX metric feature = do
  now <- window
  cs <- connectionString
  conn <- connectPostgreSQL cs
  obtainSample conn metric feature now


tableAccounts24H = do
  now <- window
  conn <- connectionString >>= connectPostgreSQL
  accResults <- mapM (\x -> obtainSample conn "account" x now) accounts
  rewardResults <- mapM (\x -> obtainSampleF conn "rewards" x now) accounts
  let tableHead = thead (th "Account Number" >> th "Balance change in UND" >> th "Accrued rewards")
  let xns = Prelude.zip3 (makeURL <$> accounts) (toHtml . undConvert <$> accResults) (toHtml <$> rewardResults)
  let rows = mapM_ (\(a, b, c) -> tr (td a >> td b >> td c)) xns
  return $
    renderHtml (table ! class_ "statstable" $ (tableHead >> rows))


tableTotalSupply24H = do
  supplyAmountChange <- metricDX "supply" "amount"
  supplyTotalChange <- metricDX "supply" "total"
  supplyLockedChange <- metricDX "supply" "locked"
  let tableHead = thead (th "Total" >> th "Amount in UND")
  let lns =
        [ ("Supply Amount Change", (toHtml . undConvert) supplyAmountChange)
        , ("Supply Total Change", (toHtml . undConvert) supplyTotalChange)
        , ("Supply Locked Change", (toHtml . undConvert) supplyLockedChange)
        ]
  return $
    renderHtml (table ! class_ "statstable" $ (tableHead >> (mapM_ c lns)))
  where
    c ln = tr (td (fst ln) >> td (snd ln))

tableValidators24H = do
  now <- window
  cs <- connectionString
  conn <- connectPostgreSQL cs
  vs <- validators conn now
  vxs <- mapM (\x -> readValidator conn x) vs
  let totalShares = sum (Parsers.Validator.shares <$> vxs)
  print $ totalShares
  let tableHead =
        thead
          (th "EV" >> th "Delegator Shares" >> th "Power %" >>
           th "Commission %")
  return $
    renderHtml
      (table ! class_ "statstable" $
       (tableHead >> (mapM_ (\x -> c totalShares x) vxs)))
  where
    shr v = truncate' (Parsers.Validator.shares v / 1000000000) 3
    pow totalShares v =
      truncate' (Parsers.Validator.shares v / totalShares * 100) 2
    comm v = truncate' (Parsers.Validator.commission v * 100) 2
    monn v =
      (makeValidatorURL
         (Parsers.Validator.address v)
         (Parsers.Validator.moniker v))
    c totalShares v =
      tr
        (td (monn v) >> td (toHtml (shr v)) >> td (toHtml (pow totalShares v)) >>
         td (toHtml (comm v)))

test = do
  now <- window
  print $ show now
  cs <- connectionString
  conn <- connectPostgreSQL cs
  obtainSampleF conn "rewards" "und10xpk56etf9s7efezvlu3qu5rg9djfm979cu4ax" now
