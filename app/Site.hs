{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid (mappend)
import           Hakyll
import           Report      (tableTotalSupply24H, tableAccounts24H)

main :: IO ()
main = do
  dataAccounts24H <- tableAccounts24H
  dataTotalSupply24H <- tableTotalSupply24H
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match "index.html" $ do
      route idRoute
      compile $ do
        let indexCtx =
              constField "title" "Stats" `mappend` constField "dataAccounts24H" dataAccounts24H `mappend` 
              constField "dataTotalSupply24H" dataTotalSupply24H `mappend` 
              defaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls
    match "templates/*" $ compile templateBodyCompiler
