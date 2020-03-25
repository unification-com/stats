{-# LANGUAGE OverloadedStrings #-}

module Report
  ( tableTotalSupply24H
  , tableAccounts24H
  ) where

import           Config                          (accounts, connectionString)
import           Data.Time.Clock                 (UTCTime, addUTCTime,
                                                  getCurrentTime)
import           Database.PostgreSQL.Simple

import           Control.Monad                   (forM_)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                as H
import           Text.Blaze.Html5.Attributes     as A

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

window :: IO (UTCTime, UTCTime)
window = do
  now <- getCurrentTime
  return $ (addUTCTime (-3600 * 24 * 1) now, now)

metricDX metric feature = do
  now <- window
  cs <- connectionString
  conn <- connectPostgreSQL cs
  obtainSample conn metric feature now

accountDX = do
  now <- window
  cs <- connectionString
  conn <- connectPostgreSQL cs
  mapM (\x -> obtainSample conn "account" x now) accounts

tableAccounts24H = do
  accResults <- accountDX
  let tableHead = thead (th "Account Number" >> th "Amount in nUND")
  let lns = zip (toHtml <$> accounts) (toHtml <$> accResults)
  return $
    renderHtml (table ! class_ "statstable" $ (tableHead >> (mapM_ c lns)))
  where
    c ln = tr (td (fst ln) >> td (snd ln))

tableTotalSupply24H = do
  supplyAmountChange <- metricDX "supply" "amount"
  supplyTotalChange <- metricDX "supply" "total"
  supplyLockedChange <- metricDX "supply" "locked"
  let tableHead = thead (th "Total" >> th "Amount in nUND")
  let lns =
        [ ("Supply Amount Change", toHtml supplyAmountChange)
        , ("Supply Total Change", toHtml supplyTotalChange)
        , ("Supply Locked Change", toHtml supplyLockedChange)
        ]
  return $
    renderHtml (table ! class_ "statstable" $ (tableHead >> (mapM_ c lns)))
  where
    c ln = tr (td (fst ln) >> td (snd ln))

test' = do
  now <- window
  print $ show now
  cs <- connectionString
  conn <- connectPostgreSQL cs
  obtainSample conn "supply" "total" $ (now)
