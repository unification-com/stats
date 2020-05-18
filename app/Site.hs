{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid (mappend)
import           Hakyll
import           Report      (tableAccounts24H, tableTotalSupply24H,
                              tableValidators24H, tableDiskUsage, writeCoreMetrics)

main :: IO ()
main = do
  writeCoreMetrics
  dataAccounts24H <- tableAccounts24H
  dataValidators24H <- tableValidators24H
  dataTotalSupply24H <- tableTotalSupply24H
  dataDiskUsage <- tableDiskUsage
  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match "index.html" $ do
      route idRoute
      compile $ do
        let indexCtx =
              constField "title" "Stats" `mappend`
              constField "dataAccounts24H" dataAccounts24H `mappend`
              constField "dataValidators24H" dataValidators24H `mappend`
              constField "dataTotalSupply24H" dataTotalSupply24H `mappend`
              constField "dataDiskUsage" dataDiskUsage `mappend`
              defaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls
    match "templates/*" $ compile templateBodyCompiler
