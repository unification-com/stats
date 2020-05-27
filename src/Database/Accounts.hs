{-# LANGUAGE OverloadedStrings #-}

module Database.Accounts
  ( allAccounts
  , Account(..)
  ) where

import           Config                             (connectionString)

import           Data.Int                           (Int64)
import           Data.Text                          (Text, pack)

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

data Account =
  Account
    { address :: String
    , name    :: String
    }
  deriving (Show)

instance FromRow Account where
  fromRow = Account <$> field <*> field

instance ToRow Account where
  toRow t = [toField (address t), toField (name t)]

allAccounts :: Connection -> IO [Account]
allAccounts c = query_ c "SELECT account, mnemonic FROM stats.accounts;"
