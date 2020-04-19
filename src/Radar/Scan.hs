module Radar.Scan
  ( scan
  , scanPorts
  ) where

import           Control.Exception          (try)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List                  (isInfixOf)
import           Network.HTTP.Simple        (HttpException, getResponseBody,
                                             httpLBS, parseRequest)
import           System.Exit                (ExitCode (ExitSuccess))
import           System.Process

type URL = String

pairs =
  [ ( "https://explorer-testnet.unification.io"
    , "Unification Mainchain Explorer")
  , ("https://faucet-testnet.unification.io", "Faucet")
  , ( "https://rest-testnet.unification.io/staking/validators"
    , "operator_address")
  , ( "http://node1-testnet.unification.io:26660/status"
    , "23be54f5d6fdf8cb49f434b9bc8762f725fe47a7")
  , ( "http://node2-testnet.unification.io:26660/status"
    , "2603d0e9d79424c8b0fa84f36dde89045cf14f53")
  , ( "http://seed1-testnet.unification.io:26660/status"
    , "dcff5de69dcc170b28b6628a1336d420f7eb60c0")
  , ("https://wallet-testnet.unification.io", "Web Wallet")
  ]

ports =
  [("172.31.38.249", 26656), ("172.31.36.108", 26656), ("172.31.33.80", 26656)]

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

netcat :: String -> Int -> IO Bool
netcat ipAddress port = do
  (_, Just hOut, _, hProc) <-
    createProcess ((shell (shell_cmd)) {std_out = CreatePipe})
  exitCode <- waitForProcess hProc
  case exitCode of
    ExitSuccess -> return True
    _           -> return False
  where
    shell_cmd = "nc " ++ ipAddress ++ " " ++ show port ++ " < /dev/null"

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
      return val

testPort :: Integer -> URL -> Int -> IO Bool
testPort 3 _ _ = return False
testPort n ipAddress port = do
  result <- try (netcat ipAddress port) :: IO (Either HttpException Bool)
  case result of
    Left ex -> do
      putStrLn $ "Caught exception: " ++ show ex
      testPort (n + 1) ipAddress port
    Right val -> do
      putStrLn $ ipAddress ++ " " ++ show val
      return val

render :: (Bool, (String, String)) -> String
render (success, (url, _)) = url ++ " " ++ show success

renderPort :: (Bool, (String, Int)) -> String
renderPort (success, (url, port)) = url ++ " " ++ show port ++ " " ++ show success

scan = do
  result <- mapM (\(url, needle) -> testSite 0 url needle) pairs
  let issue = any (\x -> x == False) result
  let c = unlines (map render (zip result pairs))
  if issue
    then return $ Left c
    else return $ Right "HTTP scan is fine"

scanPorts = do
  result <- mapM (\(ipAddress, port) -> testPort 0 ipAddress port) ports
  let issue = any (\x -> x == False) result
  let c = unlines (map renderPort (zip result ports))
  if issue
    then return $ Left c
    else return $ Right "Port scan is fine"
