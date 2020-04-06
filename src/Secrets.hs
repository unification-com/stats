module Secrets
  ( getSecret
  ) where

import           Data.List        (elemIndex)
import           System.Directory (getHomeDirectory)
import           System.FilePath  ((</>))

filename = ".secrets"

getSecret :: String -> IO (Maybe String)
getSecret key = do
  base <- getHomeDirectory
  x <- readFile (base </> filename)
  let filtered = filter (\x -> length x > 0) (lines x)
  case length filtered of
    0 -> return $ Nothing
    _ -> do
      let xs = words <$> filtered
      case elemIndex key (head <$> xs) of
        Nothing  -> return $ Nothing
        Just (x) -> return $ Just ((xs !! x) !! 1)
