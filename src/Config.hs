module Config
  ( connectionString
  , coreMetricsPath
  ) where

import           Data.List          (elemIndex)
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           System.Environment (getEnvironment)

has vars = case hasCell of
  Nothing -> Nothing
  (Just x) -> snd <$> Just (vars !! x)
  where
    hasCell = elemIndex "bits_env" (fst <$> vars)

connectionString = do
  vars <- getEnvironment
  case has vars of
    Nothing -> return (encode "postgresql://postgres:password@localhost:8432/postgres")
    (Just x) -> case x of
      "warp" -> return (encode "postgresql://indika:password@localhost:5432/warp")
      "aws" -> return (encode "postgresql://postgres:password@localhost:5432/postgres")
  where
    encode = encodeUtf8 . T.pack


coreMetricsPath = do
  vars <- getEnvironment
  case has vars of
    Nothing -> return "/tmp"
    (Just x) -> case x of
      "warp" -> return "/tmp"
      "aws" -> return "/home/deploy/src/coremetrics"
