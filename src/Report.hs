{-# LANGUAGE OverloadedStrings #-}

module Report
  ( tableTotalSupply24H
  , tableAccounts24H
  , tableValidators24H
  , tableValidators24HLite
  , tableDiskUsage
  , tableRewards
  , tableSimpleRewards
  , writeCoreMetrics
  , coreTable
  ) where

import           Control.Monad                   (forM_)
import           Data.Function                   (on)
import           Data.List                       (sortBy, zip4, zip6)
import           Data.Map.Strict                 as M (Map, fromList, keys,
                                                       lookup, toList, union)
import           Data.Text                       as T hiding (map)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime,
                                                  addUTCTime, diffUTCTime,
                                                  getCurrentTime, nominalDay)
import           Database.PostgreSQL.Simple
import           Numeric                         (showFFloat)

import           System.FilePath                 ((</>))
import           System.IO                       (Handle, IOMode (WriteMode),
                                                  hClose, hPutStrLn, openFile)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                as H hiding (address, map)
import           Text.Blaze.Html5.Attributes     as A

import           Config                          (connectionString,
                                                  coreMetricsPath)
import           Database.Accounts               as DA (Account (..),
                                                        allAccounts)
import           Parsers.Account                 (queryMainchainAccount, supply)
import           Parsers.Validator               as V (Validator (..),
                                                       readValidator)
import           Queries                         (FeatureQuery, Window,
                                                  latestZQuery, obtainSample,
                                                  obtainSampleF)
import           Renderer                        (percentage, renderTable,
                                                  undCommaSeperate,
                                                  undCommaSeperateZ,
                                                  undConvertF, undConvertZ)

validators :: Connection -> Window -> IO [String]
validators c (a, b) = do
  a <-
    query
      c
      "SELECT DISTINCT operator_address from stats.validators WHERE utc_date > ? AND utc_date < ?;" $
    [show a, show b] :: IO [Only String]
  return $ fromOnly <$> a

window :: Int -> IO Window
window days = do
  now <- getCurrentTime
  let seconds = realToFrac (-3600 * 24 * days)
  return $ (addUTCTime (-3600 * 24) now, now)

gbConvert :: Int -> String
gbConvert n = showFFloat (Just 2) (fromIntegral n / 1000000) ""

makeURL :: String -> Html
makeURL acc = a ! href (stringValue x) $ (toHtml acc)
  where
    x = "https://explorer.unification.io/account/" ++ acc

makeValidatorURL :: String -> Text -> Html
makeValidatorURL acc m = a ! href (textValue x) $ (toHtml m)
  where
    x =
      T.concat [T.pack "https://explorer.unification.io/validator/", T.pack acc]

metricDX metric feature = do
  now <- window 1
  conn <- connectionString >>= connectPostgreSQL
  obtainSample conn (metric, feature, now)

tableAccounts24H = do
  now <- window 1
  conn <- connectionString >>= connectPostgreSQL
  allAccounts <- DA.allAccounts conn
  let accounts = DA.address <$> allAccounts
  balance <- mapM (\x -> obtainSample conn ("account", x, now)) accounts
  accruedRewards <- mapM (\x -> obtainSampleF conn ("rewards", x, now)) accounts
  let totalRewards = undConvertF $ sum accruedRewards
  let headers = ["Account Number", "Balance change", "Accrued rewards"]
  let xns' =
        zip3
          (makeURL <$> accounts)
          (undConvertZ <$> balance)
          (undConvertF <$> accruedRewards)
  let xns'' =
        xns' ++
        [ ( toHtml ("Total" :: String)
          , undConvertZ $ sum balance
          , undConvertF $ sum accruedRewards)
        ]
  let xns = (\(a, b, c) -> [a, b, c]) <$> xns''
  return $ renderTable headers xns

tableTotalSupply24H = do
  supplyAmountChange <- metricDX "supply" "amount"
  supplyLockedChange <- metricDX "supply" "locked"
  supplyTotalChange <- metricDX "supply" "total"
  let headers = ["Total", "Amount in FUND"]
  let xns =
        [ ["Supply Amount Change", undConvertZ supplyAmountChange]
        , ["Supply Locked Change", undConvertZ supplyLockedChange]
        , ["Supply Total Change", undConvertZ supplyTotalChange]
        ]
  return $ renderTable headers xns

tableValidators24H = do
  now <- window 1
  conn <- connectionString >>= connectPostgreSQL
  vs <- validators conn now
  vxsRaw <- mapM (V.readValidator conn) vs
  let vxs = sortBy (flip compare `on` shares) vxsRaw
  validatorRewards <-
    mapM
      (\x -> obtainSampleF conn ("rewards_validator", x, now))
      (V.address <$> vxs)
  validatorRewardsOutstanding <-
    mapM
      (\x -> obtainSampleF conn ("rewards_outstanding_validator", x, now))
      (V.address <$> vxs)
  let sharesTotal = sum (shares <$> vxs)
  let headers =
        [ "EV"
        , "Delegator Shares"
        , "Power %"
        , "Rewards"
        , "Outstanding"
        , "Commission %"
        ]
  let xns' =
        zip6
          ((\v -> makeValidatorURL (V.address v) (moniker v)) <$> vxs)
          (undCommaSeperate . shares <$> vxs)
          ((\v -> percentage (shares v / sharesTotal * 100)) <$> vxs)
          (undConvertF <$> validatorRewards)
          (undConvertF <$> validatorRewardsOutstanding)
          ((\v -> percentage (commission v * 100)) <$> vxs)
  let xns'' =
        xns' ++
        [ ( toHtml ("Total" :: String)
          , undCommaSeperate sharesTotal
          , toHtml ("100.00" :: String)
          , toHtml ("N/A" :: String)
          , toHtml ("N/A" :: String)
          , toHtml ("N/A" :: String))
        ]
  let xns = (\(a, b, c, d, e, f) -> [a, b, c, d, e, f]) <$> xns''
  return $ renderTable headers xns

tableValidators24HLite = do
  now <- window 1
  conn <- connectionString >>= connectPostgreSQL
  vs <- validators conn now
  vxsRaw <- mapM (V.readValidator conn) vs
  let vxs = sortBy (flip compare `on` shares) vxsRaw
  let sharesTotal = sum (shares <$> vxs)
  let headers = ["EV", "Delegator Shares", "Power %", "Commission %"]
  let xns' =
        zip4
          ((\v -> makeValidatorURL (V.address v) (moniker v)) <$> vxs)
          (undCommaSeperate . shares <$> vxs)
          ((\v -> percentage (shares v / sharesTotal * 100)) <$> vxs)
          ((\v -> percentage (commission v * 100)) <$> vxs)
  let xns'' =
        xns' ++
        [ ( toHtml ("Total" :: String)
          , undCommaSeperate sharesTotal
          , toHtml ("100.00" :: String)
          , toHtml ("N/A" :: String))
        ]
  let xns = (\(a, b, c, d) -> [a, b, c, d]) <$> xns''
  return $ renderTable headers xns

zipMap :: Map String Int -> Map String Int -> Map String (Maybe Int, Maybe Int)
zipMap m1 m2 =
  let allKeys = keys (m1 `union` m2)
      f' k = (k, (M.lookup k m1, M.lookup k m2))
   in fromList $ map f' allKeys

repr :: Maybe Int -> String
repr Nothing  = "N/A"
repr (Just x) = gbConvert x

tableDiskUsage = do
  now <- window 1
  conn <- connectionString >>= connectPostgreSQL
  l1 <- latestZQuery conn ("DiskUsage", "Used", now)
  l2 <- latestZQuery conn ("DiskUsage", "1KBlocks", now)
  let m3 = zipMap (fromList l1) (fromList l2)
  let xns =
        (\(a, b) -> [toHtml a, toHtml $ repr . fst $ b, toHtml $ repr . snd $ b]) <$>
        (M.toList m3)
  let headers = ["Machine", "Used (GB)", "Total (GB)"]
  return $ renderTable headers xns

writeMetric target value = do
  basePath <- coreMetricsPath
  let totalSupplyTarget = basePath </> target
  outputHandle <- openFile totalSupplyTarget WriteMode
  hPutStrLn outputHandle value
  hClose outputHandle

quickJSON a b c =
  "{\"total-supply\":" ++
  a ++ ",\"circulating-supply\":" ++ b ++ ",\"num-validators\":" ++ c ++ "}"

writeCoreMetrics = do
  locked <- queryMainchainAccount leftOversAccount
  now <- window 1
  conn <- connectionString >>= connectPostgreSQL
  vs <- validators conn now
  vxs <- mapM (\x -> V.readValidator conn x) vs
  (amount, _, _) <- supply
  let totalSupply = read amount :: Int
  let sharesTotal = sum (shares <$> vxs)
  let circulating = totalSupply - locked
  let liquid = circulating - (round sharesTotal :: Int)
  let j =
        quickJSON
          (render totalSupply)
          (render circulating)
          (show $ Prelude.length vs)
  writeMetric "total-supply/index.html" $ render totalSupply
  writeMetric "circulating-supply/index.html" $ render circulating
  writeMetric "liquid-supply/index.html" $ render liquid
  writeMetric "landing/index.html" $ j
  where
    leftOversAccount = "und1fxnqz9evaug5m4xuh68s62qg9f5xe2vzsj44l8"
    nund = 1000000000
    render x = show (x `Prelude.div` nund)

coreTable = do
  locked <- queryMainchainAccount leftOversAccount
  now <- window 1
  conn <- connectionString >>= connectPostgreSQL
  vs <- validators conn now
  vxs <- mapM (\x -> V.readValidator conn x) vs
  (amount, _, _) <- supply
  let totalSupply = read amount :: Int
  let sharesTotal = sum (shares <$> vxs)
  let circulating = totalSupply - locked
  let sharesTotalRounded = round sharesTotal :: Int
  let liquid = circulating - sharesTotalRounded
  let headers = ["Metric", "Amount in FUND"]
  let xns =
        [ ["Total Supply", undCommaSeperateZ totalSupply]
        , ["Admin FUND", undCommaSeperateZ locked]
        , ["Circulating Supply", undCommaSeperateZ circulating]
        , ["Staked FUND", undCommaSeperateZ sharesTotalRounded]
        , ["Liquid Supply", undCommaSeperateZ liquid]
        ]
  return $ renderTable headers xns
  where
    leftOversAccount = "und1fxnqz9evaug5m4xuh68s62qg9f5xe2vzsj44l8"

tableRewards = do
  now <- window 7
  conn <- connectionString >>= connectPostgreSQL
  x <- obtainSample conn ("supply", "amount", now)
  let annual = x * 365
  let headers = ["Period", "FUND Reward"]
  let xns =
        [ ["Daily", undCommaSeperateZ x]
        , ["Annual Projection", undCommaSeperateZ annual]
        ]
  return $ Renderer.renderTable headers xns

tableSimpleRewards = do
  let genesis = (read "2020-05-14 09:33:07 UTC") :: UTCTime
  now <- getCurrentTime :: IO UTCTime
  let sub = now `diffUTCTime` genesis
  let days = sub / nominalDay
  (amount, _, _) <- supply
  let totalSupply = read amount :: Double
  let initialSupply = read "120000000000000000" :: Double
  let supplyChange = totalSupply - initialSupply
  -- TODO: Why is this computationally expensive?
  let x = supplyChange / (realToFrac days)
  let annual = x * 365
  let headers = ["Period", "FUND Reward"]
  let xns =
        [ ["Daily", undCommaSeperate x]
        , ["Annual Projection", undCommaSeperate annual]
        ]
  return $ renderTable headers xns

