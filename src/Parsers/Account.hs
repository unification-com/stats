module Parsers.Account
  ( queryMainchainAccount
  , queryRewards
  , queryValidatorRewards
  , queryValidatorOutstandingRewards
  , supply
  ) where

import           Parsers.Common (curljq, restEndpoint)
import           System.IO      (hGetContents)
import           System.Process

queryMainchainAccount account = do
  val <-
    curljq
      (restEndpoint "auth/accounts/" ++ account)
      ".result.account.value.coins[0].amount"
  if head val == "null"
    then return "0"
    else return $ head val

queryRewards account = do
  val <-
    curljq
      (restEndpoint "distribution/delegators/" ++ account ++ "/rewards")
      ".result.total[0].amount"
  if head val == "null"
    then return 0
    else return $ (read (head val) :: Float)

queryValidatorRewards account = do
  val <-
    curljq
      (restEndpoint "distribution/validators/" ++ account ++ "/rewards")
      ".result[0].amount"
  if head val == "null"
    then return 0
    else return $ (read (head val) :: Float)

queryValidatorOutstandingRewards account = do
  val <-
    curljq
      (restEndpoint "distribution/validators/" ++ account ++ "/outstanding_rewards")
      ".result[0].amount"
  if head val == "null"
    then return 0
    else return $ (read (head val) :: Float)

-- TODO: This could be optimized
supply = do
  amount <- curljq supplyEndpoint ".result.amount"
  locked <- curljq supplyEndpoint ".result.locked"
  total <- curljq supplyEndpoint ".result.total"
  return $ (head amount, head locked, head total)
  where
    supplyEndpoint = restEndpoint "supply/total"
