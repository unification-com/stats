{-# LANGUAGE QuasiQuotes #-}

module Queries
  ( latestZQuery
  , obtainSample
  , obtainSampleF
  , FeatureQuery
  , Window
  ) where

import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import           Data.Time.Clock                  (UTCTime)
import           Database.PostgreSQL.Simple       (Connection, Only, fromOnly,
                                                   query)
import           Database.PostgreSQL.Simple.Types (Query (..))
import           Text.Printf                      (printf)
import           Text.RawString.QQ

type Window = (UTCTime, UTCTime)

type FeatureQuery = (String, String, Window)

queryFromString x = Query ((encodeUtf8 . T.pack) x)

obtainSampleQuery :: String -> Query
obtainSampleQuery table =
  (queryFromString $
   (printf
      [r|

    SELECT a.sample - b.sample FROM (
        SELECT sample FROM %s
        WHERE metric = ? and feature = ? AND utc_date > ? AND utc_date < ?
        ORDER BY utc_date DESC LIMIT 1) a
    CROSS JOIN (
        SELECT sample FROM %s
        WHERE metric = ? and feature = ? AND utc_date > ? AND utc_date < ?
        ORDER BY utc_date ASC LIMIT 1) b;
    |]
      table
      table))

obtainSample :: Connection -> FeatureQuery -> IO Int
obtainSample c (metric, feature, (a, b)) = do
  a <-
    query c (obtainSampleQuery table) $
    [metric, feature, show a, show b, metric, feature, show a, show b]
  return $ fromOnly $ Prelude.head a
  where
    table = "stats.metrics"

obtainSampleF :: Connection -> FeatureQuery -> IO Double
obtainSampleF c (metric, feature, (a, b)) = do
  a <-
    query c (obtainSampleQuery table) $
    [metric, feature, show a, show b, metric, feature, show a, show b]
  return $ fromOnly $ Prelude.head a
  where
    table = "stats.metricsf"

latestZQuery :: Connection -> FeatureQuery -> IO [(String, Int)]
latestZQuery c (metric, feature, (a, b)) =
  query
    c
    (queryFromString $
     (printf
        [r|

  SELECT DISTINCT ON (N1.machine) machine, sample FROM %s N1
  INNER JOIN (
      SELECT DISTINCT(machine) as machinej FROM %s
      WHERE metric = ? AND feature = ? AND utc_date > ? AND utc_date < ?) N2
      ON (N1.machine = N2.machinej)
  ORDER BY N1.machine, utc_date DESC;
  |]
        table
        table)) $
  [metric, feature, show a, show b]
  where
    table = "stats.metrics"
