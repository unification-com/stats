module Parsers.Account
  ( queryMainchainAccount
  , queryRewards
  , supply
  ) where

import           Parsers.Common (curljq, restEndpoint)
import           System.IO      (hGetContents)
import           System.Process

data Validator =
  Validator String Integer Double Double Bool
  deriving (Show)

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

-- TODO: This could be optimized
supply = do
  amount <- curljq supplyEndpoint ".result.amount"
  locked <- curljq supplyEndpoint ".result.locked"
  total <- curljq supplyEndpoint ".result.total"
  return $ (head amount, head locked, head total)
  where
    supplyEndpoint = restEndpoint "supply/total"
