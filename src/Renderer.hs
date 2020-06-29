{-# LANGUAGE OverloadedStrings #-}

module Renderer
  ( renderTable
  , makeURL
  , percentage
  , undConvertD
  , undCommaSeperate
  ) where

import           Data.List                       (intercalate, reverse)
import           Data.List.Split                 (chunksOf)
import           Numeric                         (showFFloat)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                as H hiding (address, map)
import           Text.Blaze.Html5.Attributes     as A

undCommaSeperate :: RealFloat a => a -> Html
undCommaSeperate n = toHtml $ h ++ t
  where
    x = truncate (n / 1000000000)
    sp = break (== '.') $ show x
    h = reverse (intercalate "," $ chunksOf 3 $ reverse $ fst sp)
    t = snd sp

undConvertD :: RealFloat a => a -> Html
undConvertD n = toHtml $ showFFloat (Just 2) (n / 1000000000) ""

percentage :: RealFloat a => a -> Html
percentage n = toHtml $ showFFloat (Just 2) n ""

undConvert :: Integral a => a -> String
undConvert n = showFFloat (Just 2) (fromIntegral n / 1000000000) ""

undConvertF :: RealFloat a => a -> String
undConvertF n = showFFloat (Just 2) (n / 1000000000) ""

makeURL :: String -> Html
makeURL acc = a ! href (stringValue x) $ (toHtml acc)
  where
    x = "https://explorer.unification.io/account/" ++ acc

renderTable' :: [String] -> [[Html]] -> Html
renderTable' headers ds = table ! class_ "statstable" $ tableHead >> rows
  where
    tableHead = thead (mapM_ (th . toHtml) headers)
    rows = mapM_ (\xs -> tr (mapM_ (td) xs)) ds

renderTable :: [String] -> [[Html]] -> String
renderTable headers xs = renderHtml $ renderTable' headers xs
