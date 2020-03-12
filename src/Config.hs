module Config
  ( accounts
  , validators
  , connectionString
  ) where

import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           System.Environment (getEnvironment)

accounts =
  [ "und1nkh2dteta8drxntqp646sr6vz74lt9w9yc60pd"
  , "und1hu50gv6a55tz592tk4y7hstmxfw9y9sg4kz2wm"
  , "und19enk9tmm98sa6yzk4pwarxamkkc0nth923e8nz"
  , "und1z575nxpemg4dtcrv0rd2u5pwctjxjzsy2hyzjn"
  , "und1w2dlf0793gk3m5zk8e554stg97x7uw95xl27kw"
  , "und10ta4pvss0wg6t9yhq7k6y7lhk7v95uqg7p22nu"
  , "und1m4y8cxqhvmqltwyehtdyk3ymgwd854j7wmvc5k"
  ]

validators =
  [ "undvalcons1zahztaascyqtv0ym8gkcsmvnaugl7whg8slsux"
  , "undvalcons1wuf5pvrwpld77skgjvt20j2xr367ycm0f55avf"
  , "undvalcons1hnhzr5xfhd7uv32fvens59qda8t86jywd88prz"
  , "undvalcons170cp2v6pnwefvayxtrjh6u3kftprhd9ud5jy0c"
  ]

connectionString = do
  vars <- getEnvironment
  if elem "bits_env" (fst <$> vars)
    then return
           (encodeUtf8 . T.pack $
            "postgresql://indika:password@localhost:5432/warp")
    else return
           (encodeUtf8 . T.pack $
            "postgresql://postgres:password@localhost:8432/postgres")
