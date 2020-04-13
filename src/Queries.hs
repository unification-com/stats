{-# LANGUAGE QuasiQuotes #-}

module Queries
  ( lastestZQuery
  , FeatureQuery
  , Window
  ) where

import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import           Data.Time.Clock                  (UTCTime)
import           Database.PostgreSQL.Simple       (Connection, query)
import           Database.PostgreSQL.Simple.Types (Query (..))
import           Text.Printf                      (printf)
import           Text.RawString.QQ

type Window = (UTCTime, UTCTime)

type FeatureQuery = (String, String, String, Window)

queryFromString x = Query ((encodeUtf8 . T.pack) x)

lastestZQuery :: Connection -> FeatureQuery -> IO [(String, Int)]
lastestZQuery c (table, metric, feature, (a, b)) =
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
