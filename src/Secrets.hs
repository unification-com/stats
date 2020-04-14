module Secrets
  ( getSecret
  , getMachine
  ) where

import           Data.List        (elemIndex)
import           System.Directory (getHomeDirectory)
import           System.FilePath  ((</>))

secretsFile = ".secrets"

machineFile = ".machine"

getSecret :: String -> IO (Maybe String)
getSecret key = do
  base <- getHomeDirectory
  x <- readFile (base </> secretsFile)
  let filtered = filter (\x -> length x > 0) (lines x)
  case length filtered of
    0 -> return $ Nothing
    _ -> do
      let xs = words <$> filtered
      case elemIndex key (head <$> xs) of
        Nothing  -> return $ Nothing
        Just (x) -> return $ Just ((xs !! x) !! 1)

getMachine :: IO (Maybe String)
getMachine = do
  base <- getHomeDirectory
  x <- readFile (base </> machineFile)
  let filtered = filter (\x -> length x > 0) (lines x)
  case length filtered of
    0 -> return $ Nothing
    _ -> do
      let xs = words (head filtered)
      case length xs of
        0 -> return $ Nothing
        _ -> return $ Just (head xs)
