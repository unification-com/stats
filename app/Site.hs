{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid (mappend)
import           Hakyll

import           Report      (coreTable, tableAccounts24H, tableDiskUsage,
                              tableTotalSupply24H, tableValidators24H,
                              tableValidators24HLite, tableRewards, writeCoreMetrics)
import           Richlist    (snapshotTime, tableRichlist)

main :: IO ()
main = do
  writeCoreMetrics
  dataRichlist <- tableRichlist
  dataSnapshotTime <- snapshotTime
  dataCoreTable <- coreTable
  dataRewards <- tableRewards
  dataAccounts24H <- tableAccounts24H
  dataValidators24H <- tableValidators24H
  dataValidators24H <- tableValidators24HLite
  dataTotalSupply24H <- tableTotalSupply24H
  dataDiskUsage <- tableDiskUsage
  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match "internal.html" $ do
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
          loadAndApplyTemplate "templates/internal.html" indexCtx >>=
          relativizeUrls
    match "external.html" $ do
      route idRoute
      compile $ do
        let indexCtx =
              constField "title" "Stats" `mappend`
              constField "dataSnapshotTime" dataSnapshotTime `mappend`
              constField "dataCoreTable" dataCoreTable `mappend`
              constField "dataValidators24H" dataValidators24H `mappend`
              constField "dataRewards" dataRewards `mappend`
              constField "dataRichlist" dataRichlist `mappend`
              defaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/external.html" indexCtx >>=
          relativizeUrls
    match "templates/*" $ compile templateBodyCompiler
