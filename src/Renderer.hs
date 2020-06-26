{-# LANGUAGE OverloadedStrings #-}

module Renderer
  ( renderTable
  ) where

import           Data.Time.Clock.POSIX           (posixSecondsToUTCTime)
import           Numeric                         (showFFloat)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                as H hiding (address, map)
import           Text.Blaze.Html5.Attributes     as A

undConvertD :: RealFloat a => a -> String
undConvertD n = showFFloat (Just 2) (n / 1000000000) ""

percentage :: RealFloat a => a -> String
percentage n = showFFloat (Just 2) n ""

undConvert :: Integral a => a -> String
undConvert n = showFFloat (Just 2) (fromIntegral n / 1000000000) ""

undConvertF :: RealFloat a => a -> String
undConvertF n = showFFloat (Just 2) (n / 1000000000) ""

makeURL :: String -> Html
makeURL acc = a ! href (stringValue x) $ (toHtml acc)
  where
    x = "https://explorer.unification.io/account/" ++ acc

renderTable' :: [String] -> [[String]] -> Html
renderTable' headers ds = table ! class_ "statstable" $ tableHead >> rows
  where
    tableHead = thead (mapM_ (th . toHtml) headers)
    rows = mapM_ (\xs -> tr (mapM_ (td . toHtml) xs)) ds

renderTable :: [String] -> [[String]] -> String
renderTable headers xs = renderHtml $ renderTable' headers xs
