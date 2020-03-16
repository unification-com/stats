{-# LANGUAGE OverloadedStrings #-}

module Parsers.Validator
  ( sampleValidators
  ) where

import           Data.Int                           (Int64)

import           Config                             (accounts, connectionString)
import           Parsers.Common                     (curljq, restEndpoint)

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

data Validator =
  Validator
    { address    :: String
    , tokens     :: Integer
    , shares     :: Double
    , commission :: Double
    , jailed     :: Bool
    }
  deriving (Show)

instance ToRow Validator where
  toRow t =
    [ toField (address t)
    , toField (tokens t)
    , toField (shares t)
    , toField (commission t)
    , toField (jailed t)
    ]

addValidator :: Connection -> Validator -> IO Int64
addValidator c validator =
  execute
    c
    "INSERT INTO stats.validators (operator_address, tokens, delegator_shares, rate, jailed) VALUES (?, ?, ?, ?, ?)"
    validator

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

validators :: IO [Validator]
validators = do
  ret <-
    curljq
      validatorEndpoint
      ".result[] | .operator_address,.tokens,.delegator_shares,.commission.commission_rates.rate,.jailed"
  let v =
        map
          (\x ->
             Validator
               (x !! 0)
               (read (x !! 1) :: Integer)
               (read (x !! 2) :: Double)
               (read (x !! 3) :: Double)
               (if ((x !! 4) == "true")
                  then True
                  else False))
          (group 5 ret)
  return $ v
  where
    validatorEndpoint = restEndpoint "staking/validators"

sampleValidators = do
  v <- validators
  cs <- connectionString
  conn <- connectPostgreSQL cs
  mapM_ (addValidator conn) v
