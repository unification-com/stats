{-# LANGUAGE OverloadedStrings #-}

module Sampler
  ( sample
  ) where

import           Config                     (accounts, connectionString)
import           Parsers.Account            (queryMainchainAccount, queryRewards, supply)
import           Parsers.Validator          (sampleValidators)

import           Database.PostgreSQL.Simple

insertMetric =
  "INSERT INTO stats.metrics (metric, feature, sample) VALUES (?, ?, ?);"

insertMetricF =
  "INSERT INTO stats.metricsf (metric, feature, sample) VALUES (?, ?, ?);"

inject metric account nund = do
  cs <- connectionString
  conn <- connectPostgreSQL cs
  execute conn insertMetric $ [metric, account, nund]

injectF metric account sample = do
  cs <- connectionString
  conn <- connectPostgreSQL cs
  execute conn insertMetricF $ [metric, account, show sample]

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
  inject "account" account balance
  print $ account ++ ": " ++ balance
  rewards <- queryRewards account
  injectF "rewards" account rewards
  print $ account ++ ": " ++ show rewards

sample = do
  mapM_ queryAndInject accounts
  injectSupply
  sampleValidators
