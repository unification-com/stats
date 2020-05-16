{-# LANGUAGE OverloadedStrings #-}

module Report
  ( tableTotalSupply24H
  , tableAccounts24H
  , tableValidators24H
  , tableDiskUsage
  ) where

import           Control.Monad                   (forM_)
import           Data.List                       (zip6)
import           Data.Map.Strict                 as M (Map, fromList, keys,
                                                       lookup, toList, union)
import           Data.Text                       as T hiding (map)
import           Data.Time.Clock                 (UTCTime, addUTCTime,
                                                  getCurrentTime)
import           Database.PostgreSQL.Simple
import           Numeric                         (showFFloat)

import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                as H hiding (address, map)
import           Text.Blaze.Html5.Attributes     as A

import           Config                          (accounts, connectionString)
import           Parsers.Validator               (Validator (..), readValidator)
import           Queries                         (FeatureQuery, Window,
                                                  latestZQuery, obtainSample,
                                                  obtainSampleF)

validators :: Connection -> Window -> IO [String]
validators c (a, b) = do
  a <-
    query
      c
      "SELECT DISTINCT operator_address from stats.validators WHERE utc_date > ? AND utc_date < ?;" $
    [show a, show b] :: IO [Only String]
  return $ fromOnly <$> a

window :: IO Window
window = do
  now <- getCurrentTime
  return $ (addUTCTime (-3600 * 24) now, now)

undConvert :: Integral a => a -> String
undConvert n = showFFloat (Just 2) (fromIntegral n / 1000000000) ""

undConvertF :: RealFloat a => a -> String
undConvertF n = showFFloat (Just 2) (n / 1000000000) ""

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
      T.concat
        [ T.pack "https://explorer.unification.io/validator/"
        , T.pack acc
        ]

metricDX metric feature = do
  now <- window
  conn <- connectionString >>= connectPostgreSQL
  obtainSample conn (metric, feature, now)

tableAccounts24H = do
  now <- window
  conn <- connectionString >>= connectPostgreSQL
  balance <- mapM (\x -> obtainSample conn ("account", x, now)) accounts
  accruedRewards <- mapM (\x -> obtainSampleF conn ("rewards", x, now)) accounts
  let totalBalance = toHtml $ undConvert $ sum balance
  let totalRewards = toHtml $ undConvertF $ sum accruedRewards
  let tableHead =
        thead
          (th "Account Number" >> th "Balance change" >> th "Accrued rewards")
  let xns =
        Prelude.zip3
          (makeURL <$> accounts)
          (toHtml . undConvert <$> balance)
          ((\x -> toHtml (undConvertF x)) <$> accruedRewards)
  let rows = mapM_ (\(a, b, c) -> tr (td a >> td b >> td c)) xns
  let totals = tr (td "Total" >> td totalBalance >> td totalRewards)
  return $
    renderHtml (table ! class_ "statstable" $ (tableHead >> rows >> totals))

tableTotalSupply24H = do
  supplyAmountChange <- metricDX "supply" "amount"
  supplyLockedChange <- metricDX "supply" "locked"
  supplyTotalChange <- metricDX "supply" "total"
  let tableHead = thead (th "Total" >> th "Amount in FUND")
  let lns =
        [ ("Supply Amount Change", (toHtml . undConvert) supplyAmountChange)
        , ("Supply Locked Change", (toHtml . undConvert) supplyLockedChange)
        , ("Supply Total Change", (toHtml . undConvert) supplyTotalChange)
        ]
  return $
    renderHtml (table ! class_ "statstable" $ (tableHead >> (mapM_ c lns)))
  where
    c ln = tr (td (fst ln) >> td (snd ln))

tableValidators24H = do
  now <- window
  conn <- connectionString >>= connectPostgreSQL
  vs <- validators conn now
  vxs <- mapM (\x -> readValidator conn x) vs
  validatorRewards <-
    mapM
      (\x -> obtainSampleF conn ("rewards_validator", x, now))
      (address <$> vxs)
  validatorRewardsOutstanding <-
    mapM
      (\x -> obtainSampleF conn ("rewards_outstanding_validator", x, now))
      (address <$> vxs)
  let sharesTotal = sum (shares <$> vxs)
  let sharesTotalStr = toHtml $ undConvertF $ sharesTotal
  let tableHead =
        thead
          (th "EV" >> th "Delegator Shares" >> th "Power %" >> th "Rewards" >>
           th "Outstanding" >>
           th "Commission %")
  let totals =
        tr
          (td "Total" >> td sharesTotalStr >> td "100.00" >> td "N/A" >>
           td "N/A" >>
           td "N/A")
  let xns =
        zip6
          (map (\v -> makeValidatorURL (address v) (moniker v)) vxs)
          (map
             (\v -> toHtml $ showFFloat (Just 2) (shares v / 1000000000) "")
             vxs)
          (map
             (\v ->
                toHtml $ showFFloat (Just 2) (shares v / sharesTotal * 100) "")
             vxs)
          (toHtml . undConvertF <$> validatorRewards)
          (toHtml . undConvertF <$> validatorRewardsOutstanding)
          (map (\v -> toHtml $ showFFloat (Just 2) (commission v * 100) "") vxs)
  let rows =
        mapM_
          (\(a, b, c, d, e, f) ->
             tr (td a >> td b >> td c >> td d >> td e >> td f))
          xns
  return $
    renderHtml (table ! class_ "statstable" $ tableHead >> rows >> totals)

zipMap :: Map String Int -> Map String Int -> Map String (Maybe Int, Maybe Int)
zipMap m1 m2 =
  let allKeys = keys (union m1 m2)
      f' k = (k, (M.lookup k m1, M.lookup k m2))
   in fromList $ map f' allKeys

repr :: Maybe Int -> String
repr Nothing  = "N/A"
repr (Just x) = gbConvert x

renderTable :: [String] -> [[ String ]] -> IO String
renderTable headers ds = do
  let tableHead = thead (mapM_ (th . toHtml) headers)
  let rows = mapM_ (\xs -> tr (mapM_ (td . toHtml) xs) ) ds
  return $ renderHtml (table ! class_ "statstable" $ tableHead >> rows)

tableDiskUsage = do
  now <- window
  conn <- connectionString >>= connectPostgreSQL
  l1 <- latestZQuery conn ("DiskUsage", "Used", now)
  l2 <- latestZQuery conn ("DiskUsage", "1KBlocks", now)
  let m3 = zipMap (fromList l1) (fromList l2)
  let xns = (\(a, b) -> [a, repr . fst $ b, repr . snd $ b] ) <$> (M.toList m3)
  let headers = ["Machine", "Used (GB)", "Total (GB)"]
  t <- renderTable headers xns
  return $ t
