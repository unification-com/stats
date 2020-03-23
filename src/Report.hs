{-# LANGUAGE OverloadedStrings #-}

module Report
  ( totalSupplyDX
  ) where

import           Config                      (accounts, connectionString)
import           Data.Time.Clock             (UTCTime, addUTCTime,
                                              getCurrentTime)
import           Database.PostgreSQL.Simple

import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

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
  return $ (addUTCTime (-3600 * 24) now, now)

totalSupplyDX = do
  now <- window
  cs <- connectionString
  conn <- connectPostgreSQL cs
  obtainSample conn "supply" "total" now

accountDX = do
  now <- window
  cs <- connectionString
  conn <- connectPostgreSQL cs
  mapM (\x -> obtainSample conn "account" x now) accounts

test = do
  now <- window
  print $ show now
  cs <- connectionString
  conn <- connectPostgreSQL cs
  obtainSample conn "supply" "total" $ (now)
