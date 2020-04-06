module Radar.Scan where

import           Control.Exception          (try)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List                  (isInfixOf)
import           Network.HTTP.Simple        (HttpException, getResponseBody,
                                             httpLBS, parseRequest)

type URL = String

pairs =
  [ ( "https://explorer-testnet.unification.io"
    , "Unification Mainchain Explorer")
  , ("https://faucet-testnet.unification.io", "Faucet")
  , ( "https://rest-testnet.unification.io/staking/validators"
    , "operator_address")
  , ( "http://sentinel1-testnet.unification.io:26660/status"
    , "23be54f5d6fdf8cb49f434b9bc8762f725fe47a7")
  , ( "http://sentinel2-testnet.unification.io:26660/status"
    , "2603d0e9d79424c8b0fa84f36dde89045cf14f53")
  , ( "http://seed1-testnet.unification.io:26660/status"
    , "dcff5de69dcc170b28b6628a1336d420f7eb60c0")
  , ("https://wallet-testnet.unification.io", "Web Wallet")
  ]

fetchURL :: URL -> IO L8.ByteString
fetchURL url = do
  initReq <- parseRequest url
  response <- httpLBS initReq
  return $ getResponseBody response

check :: URL -> String -> IO Bool
check url component = do
  body <- fetchURL url
  let exists = component `isInfixOf` (L8.unpack body)
  return $ exists

-- The first var is the current iteration of the test
testSite :: Integer -> URL -> String -> IO Bool
testSite 3 _ _ = return False
testSite n url needle = do
  result <- try (check url needle) :: IO (Either HttpException Bool)
  case result of
    Left ex -> do
      putStrLn $ "Caught exception: " ++ show ex
      testSite (n + 1) url needle
    Right val -> do
      putStrLn $ url ++ " " ++ show val
      return True

render :: (Bool, (String, String)) -> String
render (success, (url, needle)) = url ++ " " ++ show success

scan :: IO (Either String String)
scan = do
  result <- mapM (\(url, needle) -> testSite 0 url needle) pairs
  let issue = any (\x -> x == False) result
  let c = unlines (map render (zip result pairs))
  case issue of
      True -> return $ Left c
      False -> return $ Right "Everything is fine"
