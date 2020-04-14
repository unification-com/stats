{-# LANGUAGE OverloadedStrings #-}

module Sampler
  ( sample
  , injectZ
  , injectF
  , injectS
  ) where

import           Config                     (accounts, connectionString)
import           Parsers.Account            (queryMainchainAccount,
                                             queryRewards,
                                             queryValidatorOutstandingRewards,
                                             queryValidatorRewards, supply)
import           Parsers.Validator          (sampleValidators)

import           Database.PostgreSQL.Simple
import           GHC.Int                    (Int64)

type Machine = String

insertMetric =
  "INSERT INTO stats.metrics (metric, feature, sample, machine) VALUES (?, ?, ?, ?);"

insertMetricF =
  "INSERT INTO stats.metricsf (metric, feature, sample, machine) VALUES (?, ?, ?, ?);"

insertMetricS =
  "INSERT INTO stats.strings (metric, feature, sample, machine) VALUES (?, ?, ?, ?);"

machineStr m =
  case m of
    Nothing -> "general"
    Just x  -> x

injectZ :: Maybe Machine -> String -> String -> Int -> IO Int64
injectZ machine metric account sample = do
  cs <- connectionString
  conn <- connectPostgreSQL cs
  execute conn insertMetric $ [metric, account, show sample, machineStr machine]

injectF :: Maybe Machine -> String -> String -> Float -> IO Int64
injectF machine metric account sample = do
  cs <- connectionString
  conn <- connectPostgreSQL cs
  execute conn insertMetricF $
    [metric, account, show sample, machineStr machine]

injectS :: Maybe Machine -> String -> String -> String -> IO Int64
injectS machine metric key sample = do
  cs <- connectionString
  conn <- connectPostgreSQL cs
  execute conn insertMetricS $ [metric, key, sample, machineStr machine]

injectSupply = do
  cs <- connectionString
  conn <- connectPostgreSQL cs
  (amount, locked, total) <- supply
  execute conn insertMetric $ ["supply", "amount", amount, machineStr Nothing]
  execute conn insertMetric $ ["supply", "locked", locked, machineStr Nothing]
  execute conn insertMetric $ ["supply", "total", total, machineStr Nothing]

mark str = do
  cs <- connectionString
  conn <- connectPostgreSQL cs
  execute conn "INSERT INTO stats.markers (marker) VALUES (?);" $
    [str :: String]

queryAndInjectAccountDetails account = do
  balance <- queryMainchainAccount account
  injectZ Nothing "account" account balance
  print $ account ++ ": " ++ (show balance)
  rewards <- queryRewards account
  injectF Nothing "rewards" account rewards
  print $ account ++ ": " ++ show rewards

queryAndInjectValidatorDetails validatorAccount = do
  rewards <- queryValidatorRewards validatorAccount
  injectF Nothing "rewards_validator" validatorAccount rewards
  print $ validatorAccount ++ ": " ++ show rewards
  rewardsOutstanding <- queryValidatorOutstandingRewards validatorAccount
  injectF
    Nothing
    "rewards_outstanding_validator"
    validatorAccount
    rewardsOutstanding
  print $ validatorAccount ++ ": " ++ show rewardsOutstanding

sample = do
  mapM_ queryAndInjectAccountDetails accounts
  injectSupply
  vs <- sampleValidators
  mapM_ queryAndInjectValidatorDetails vs
