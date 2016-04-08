module Main where

import App.Types (AllEffects)
import App.Routes (match)
import App.Layout (Action(PageView), State, view, update)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Prelude (bind, return)
import Pux (App, start, renderToDOM)
import Pux.Router (sampleUrl)
import Signal ((~>))

-- | Entry point for the browser.
main :: State -> Eff (AllEffects (dom :: DOM)) (App State Action)
main state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleUrl

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  app <- start
    { initialState: state
    , update: update
    , view: view
    , inputs: [routeSignal] }

  renderToDOM "#app" app.html

  return app
