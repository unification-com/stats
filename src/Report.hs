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
import           Numeric                         (showFFloat)

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

obtainSampleF ::
     Connection -> String -> String -> (UTCTime, UTCTime) -> IO Double
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

undConvert :: Integral a => a -> String
undConvert n = showFFloat (Just 2) (fromIntegral n / 1000000000) ""

undConvertF :: RealFloat a => a -> String
undConvertF n = showFFloat (Just 2) (n / 1000000000) ""

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
  conn <- connectionString >>= connectPostgreSQL
  obtainSample conn metric feature now

tableAccounts24H = do
  now <- window
  conn <- connectionString >>= connectPostgreSQL
  balance <- mapM (\x -> obtainSample conn "account" x now) accounts
  accruedRewards <- mapM (\x -> obtainSampleF conn "rewards" x now) accounts
  let totalBalance = toHtml $ undConvert $ sum balance
  let totalRewards = toHtml $ undConvertF $ sum accruedRewards
  let tableHead =
        thead
          (th "Account Number" >> th "Balance change" >> th "Accrued rewards")
  let xns =
        Prelude.zip3
          (makeURL <$> accounts)
          (toHtml . undConvert <$> balance)
          ((\x -> toHtml (undConvertF x)) <$> accruedRewards)
  let rows = mapM_ (\(a, b, c) -> tr (td a >> td b >> td c)) xns
  let totals = tr (td "Total" >> td totalBalance >> td totalRewards)
  return $
    renderHtml (table ! class_ "statstable" $ (tableHead >> rows >> totals))

tableTotalSupply24H = do
  supplyAmountChange <- metricDX "supply" "amount"
  supplyLockedChange <- metricDX "supply" "locked"
  supplyTotalChange <- metricDX "supply" "total"
  let tableHead = thead (th "Total" >> th "Amount in UND")
  let lns =
        [ ("Supply Amount Change", (toHtml . undConvert) supplyAmountChange)
        , ("Supply Locked Change", (toHtml . undConvert) supplyLockedChange)
        , ("Supply Total Change", (toHtml . undConvert) supplyTotalChange)
        ]
  return $
    renderHtml (table ! class_ "statstable" $ (tableHead >> (mapM_ c lns)))
  where
    c ln = tr (td (fst ln) >> td (snd ln))

tableValidators24H = do
  now <- window
  conn <- connectionString >>= connectPostgreSQL
  vs <- validators conn now
  vxs <- mapM (\x -> readValidator conn x) vs
  let totalShares = sum (Parsers.Validator.shares <$> vxs)
  let totalSharesStr = toHtml $ undConvertF $ totalShares
  let tableHead =
        thead
          (th "EV" >> th "Delegator Shares" >> th "Power %" >> th "Commission %")
  let totals = tr (td "Total" >> td totalSharesStr >> td "100.00" >> td "N/A")
  return $
    renderHtml
      (table ! class_ "statstable" $
       (tableHead >> (mapM_ (\x -> c totalShares x) vxs)) >> totals)
  where
    shr v = showFFloat (Just 2) (Parsers.Validator.shares v / 1000000000) ""
    pow totalShares v =
      showFFloat (Just 2) (Parsers.Validator.shares v / totalShares * 100) ""
    comm v = showFFloat (Just 2) (Parsers.Validator.commission v * 100) ""
    monn v =
      (makeValidatorURL
         (Parsers.Validator.address v)
         (Parsers.Validator.moniker v))
    c totalShares v =
      tr
        (td (monn v) >> td (toHtml (shr v)) >> td (toHtml (pow totalShares v)) >>
         td (toHtml (comm v)))
