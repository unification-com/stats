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
  [ ( "https://explorer.unification.io"
    , "Unification Mainchain Explorer")
  , ( "https://rest.unification.io/staking/validators"
    , "operator_address")
  , ( "https://rest2.unification.io/staking/validators"
    , "operator_address")
  , ( "https://api-tokenswap.unification.io/pingtokenswap"
    , "bnb1hgk73jsfcg9achdmdrtn3h4pprjemfdhpdh3pn")
  , ( "https://api-tokenswap.unification.io/pingtokenswap"
    , "0x82FA9fbca5d6e31fC8531D3A8cF684552288d66F")
  , ( "https://docs.unification.io"
    , "Unification Mainchain Documentation")
  ]

ports =
  [ ("172.31.37.186", 26656)
  , ("172.31.41.60", 26656)
  , ("10.0.0.94", 26656)
  , ("10.0.0.65", 26656)
  , ("172.31.40.139", 26656)
  , ("172.31.39.250", 26656)
  , ("10.0.0.219", 26656)
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
