{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Queries
  ( lastestZQuery
  ) where

import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import           Database.PostgreSQL.Simple.Types (Query (..))
import           Text.RawString.QQ

lastestZQuery :: Query
lastestZQuery =
  Query
    ((encodeUtf8 . T.pack)
       [r|

SELECT DISTINCT ON (N1.machine) machine, sample FROM stats.metrics N1
INNER JOIN (
    SELECT DISTINCT(machine) as machinej FROM stats.metrics
    WHERE metric = ? AND feature = ? AND utc_date > ? AND utc_date < ?) N2
    ON (N1.machine = N2.machinej)
ORDER BY N1.machine, utc_date DESC;
|])
