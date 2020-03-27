{-# LANGUAGE OverloadedStrings #-}

module Parsers.Validator
  ( sampleValidators, readValidator, Validator(..)
  ) where

import           Data.Int                           (Int64)
import           Data.Text                          (Text, pack)

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
    , moniker    :: Text
    }
  deriving (Show)

instance FromRow Validator where
  fromRow = Validator <$> field <*> field <*> field <*> field <*> field  <*> field

instance ToRow Validator where
  toRow t =
    [ toField (address t)
    , toField (tokens t)
    , toField (shares t)
    , toField (commission t)
    , toField (jailed t)
    , toField (moniker t)
    ]

addValidator :: Connection -> Validator -> IO Int64
addValidator c validator =
  execute
    c
    "INSERT INTO stats.validators (operator_address, tokens, delegator_shares, rate, jailed, moniker) VALUES (?, ?, ?, ?, ?, ?)"
    validator

readValidator :: Connection -> String -> IO Validator
readValidator c operatorAddress = do
  a <- query
    c
    "SELECT operator_address, tokens, delegator_shares, rate, jailed, moniker from stats.validators WHERE operator_address = ? ORDER BY utc_date DESC LIMIT 1;" $ Only operatorAddress
  return $ Prelude.head a

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
      ".result[] | .operator_address,.tokens,.delegator_shares,.commission.commission_rates.rate,.jailed,.description.moniker"
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
                  else False)
               (pack (x !! 5)))
          (group 6 ret)
  return $ v
  where
    validatorEndpoint = restEndpoint "staking/validators"


test = do
  cs <- connectionString
  conn <- connectPostgreSQL cs
  readValidator conn "undvaloper12t657d878j97c3qn55r4zwx3smmwd3rxj953ku"

sampleValidators = do
  v <- validators
  cs <- connectionString
  conn <- connectPostgreSQL cs
  mapM_ (addValidator conn) v
