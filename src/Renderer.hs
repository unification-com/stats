{-# LANGUAGE OverloadedStrings #-}

module Renderer
  ( renderTable
  , makeURL
  , percentage
  , undConvertD
  , undCommaSeperate
  , undCommaSeperateZ
  ) where

import           Data.List                       (intercalate, reverse)
import           Data.List.Split                 (chunksOf)
import           Numeric                         (showFFloat)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                as H hiding (address, map)
import           Text.Blaze.Html5.Attributes     as A

commaSeperate x = h ++ t
  where
    sp = break (== '.') $ show x
    h = reverse (intercalate "," $ chunksOf 3 $ reverse $ fst sp)
    t = snd sp

undCommaSeperate :: RealFloat a => a -> Html
undCommaSeperate n = toHtml $ commaSeperate (truncate (n / 1000000000))

undCommaSeperateZ :: Int -> Html
undCommaSeperateZ n = toHtml $ commaSeperate (n `Prelude.div` 1000000000)

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
