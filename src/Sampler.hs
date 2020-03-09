{-# LANGUAGE OverloadedStrings #-}

module Sampler
  ( sample
  ) where

import           Config                     (accounts, connectionString)
import           Parsers.Account            (queryMainchainAccount, supply)

import           Database.PostgreSQL.Simple

insertMetric =
  "INSERT INTO stats.metrics (metric, feature, sample) VALUES (?, ?, ?);"

inject account nund = do
  cs <- connectionString
  conn <- connectPostgreSQL cs
  execute conn insertMetric $ ["account", account, nund]

injectSupply = do
  cs <- connectionString
  conn <- connectPostgreSQL cs
  (amount, locked, total) <- supply
  execute conn insertMetric $ ["supply", "amount", amount]
  execute conn insertMetric $ ["supply", "locked", locked]
  execute conn insertMetric $ ["supply", "total", total]

mark str = do
  cs <- connectionString
  conn <- connectPostgreSQL cs
  execute conn "INSERT INTO stats.markers (marker) VALUES (?);" $
    [str :: String]

queryAndInject account = do
  balance <- queryMainchainAccount account
  inject account balance
  print $ account ++ ": " ++ balance

sample = do
  mapM_ queryAndInject accounts
  injectSupply
