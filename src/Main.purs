module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen (parentState, runUI)
import Halogen.Util (awaitBody, runHalogenAff)

import Humblr.Components.Dashboard
import Humblr.Config

main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    driver <- runUI dashboardComponent (parentState initialDashboardState) body
    pure unit
