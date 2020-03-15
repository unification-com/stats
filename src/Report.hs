{-# LANGUAGE OverloadedStrings #-}

module Report
  ( report
  ) where

import           Config                     (accounts, connectionString)
import           Data.Time.Clock            (UTCTime, addUTCTime)
import           Database.PostgreSQL.Simple

start = "2020-03-08 09:37:25 UTC"

mid = "2020-03-08 13:23:25 UTC"

obtainSample :: Connection -> String -> String -> (UTCTime, UTCTime) -> IO Int
obtainSample c metric feature (a, b) = do
  a <-
    query
      c
      "SELECT sample FROM stats.metrics WHERE metric = ? AND feature = ? AND utc_date > ? and utc_date < ? LIMIT 1;" $
    [metric, feature, show a, show b] :: IO [Only Int]
  return $ fromOnly $ head a

window :: String -> (UTCTime, UTCTime)
window point = (a, b)
  where
    ts = (read point) :: UTCTime
    a = addUTCTime (-1 * d) ts
    b = addUTCTime (1 * d) ts
    d = 10

makeQuery = do
  cs <- connectionString
  conn <- connectPostgreSQL cs
  a <- mapM (\x -> obtainSample conn "account" x $ window start) accounts
  b <- mapM (\x -> obtainSample conn "account" x $ window mid) accounts
  let c = zip b a
  let d = map (\x -> (fst x - snd x) `div` 1000000000) c
  return $ d

report :: IO String
report = show <$> makeQuery
