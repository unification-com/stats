{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid (mappend)
import           Hakyll
import           Report      (report)

main :: IO ()
main = do
  ret <- report
  hakyll $ do
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match "index.html" $ do
      route idRoute
      compile $ do
        let indexCtx =
              constField "title" "Stats" `mappend` constField "data" ret `mappend`
              defaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls
    match "templates/*" $ compile templateBodyCompiler
