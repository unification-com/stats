module Config
  ( accounts
  , connectionString
  ) where

import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           System.Environment (getEnv)

accounts =
  [ "und1nkh2dteta8drxntqp646sr6vz74lt9w9yc60pd"
  , "und1hu50gv6a55tz592tk4y7hstmxfw9y9sg4kz2wm"
  , "und19enk9tmm98sa6yzk4pwarxamkkc0nth923e8nz"
  , "und1z575nxpemg4dtcrv0rd2u5pwctjxjzsy2hyzjn"
  ]

connectionString = do
  env <- getEnv "bits_env"
  if env == "warp" then return (encodeUtf8 . T.pack $ "postgresql://indika:password@localhost:5432/warp")
  else return (encodeUtf8 . T.pack $ "undefined")
