module Radar.Scan
  ( scan
  , scanMetrics
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
  [ ("https://explorer.unification.io", "Unification Mainchain Explorer")
  , ("https://rest.unification.io/staking/validators", "operator_address")
  , ("https://rest2.unification.io/staking/validators", "operator_address")
  , ("https://rpc1.unification.io/status", "rpcrest")
  , ("https://rpc2.unification.io/status", "rpcrest")
  , ( "https://api-tokenswap.unification.io/pingtokenswap"
    , "bnb1hgk73jsfcg9achdmdrtn3h4pprjemfdhpdh3pn")
  , ( "https://api-tokenswap.unification.io/pingtokenswap"
    , "0x82FA9fbca5d6e31fC8531D3A8cF684552288d66F")
  , ("https://docs.unification.io", "Unification Mainchain Documentation")
  ]

ports =
  [ ("18.222.228.70", 26656)
  , ("18.188.19.204", 26656)
  , ("18.216.27.230", 26656)
  , ("18.218.202.104", 26656)
  , ("18.191.81.153", 26656)
  , ("18.216.171.197", 26656)
  , ("3.234.218.211", 26656)
  , ("18.210.24.149", 26656)
  , ("34.225.194.28", 26656)
  , ("18.204.37.53", 26656)
  , ("3.233.234.176", 26656)
  , ("18.204.2.16", 26656)
  , ("172.31.44.118", 26656)
  , ("172.31.46.108", 26656)
  , ("172.31.35.91", 26656)
  , ("172.31.35.108", 26656)
  , ("172.31.45.34", 26656)
  , ("172.31.40.227", 26656)
  , ("10.0.0.212", 26656)
  , ("10.0.0.176", 26656)
  , ("10.0.0.201", 26656)
  , ("10.0.0.80", 26656)
  , ("10.0.0.189", 26656)
  , ("10.0.0.225", 26656)
  , ("3.19.240.58", 26656)
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

netcat :: String -> Int -> IO Bool
netcat ipAddress port = do
  (_, Just hOut, _, hProc) <-
    createProcess ((shell shell_cmd) {std_out = CreatePipe})
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
  result <- netcat ipAddress port
  case result of
    False -> do
      putStrLn $ ipAddress ++ " " ++ show port ++ " Failed"
      testPort (n + 1) ipAddress port
    True -> return True

render :: (Bool, (String, String)) -> String
render (success, (url, _)) = url ++ " " ++ show success

renderPort :: (Bool, (String, Int)) -> String
renderPort (success, (url, port)) =
  url ++ " " ++ show port ++ " " ++ show success

fetchCoreMetrics :: IO (Int, Int, Int)
fetchCoreMetrics = do
  total <- fetchURL "https://stats.unification.io/total-supply"
  circulating <- fetchURL "https://stats.unification.io/circulating-supply/"
  liquid <- fetchURL "https://stats.unification.io/liquid-supply/"
  let totalInt = read (L8.unpack total) :: Int
  let circulatingInt = read (L8.unpack circulating) :: Int
  let liquidInt = read (L8.unpack liquid) :: Int
  return $ (totalInt, circulatingInt, liquidInt)

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

scanMetrics = do
  (totalInt, circulatingInt, liquidInt) <- fetchCoreMetrics
  let nonIssue = (totalInt > 120000000) && (circulatingInt > liquidInt)
  if nonIssue
    then return $ Right "Core Metric scan is fine"
    else return $ Left "Core Metric scan has an issue"
