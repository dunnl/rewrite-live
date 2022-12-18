module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM.Client (createRoot, renderRoot)
import RewriteLive.App (renderApp)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  container <- getElementById "app" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "RW: App root element not found. I'm looking for something like '<div id=app>'"
    Just c  -> do
      root <- createRoot c
      renderRoot root (renderApp unit)
