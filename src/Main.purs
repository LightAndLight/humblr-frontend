module Main where

import Prelude (Unit, unit, pure, bind)
import Control.Monad.Eff (Eff)
import Halogen (parentState, runUI)
import Halogen.Util (awaitBody, runHalogenAff)

import Humblr.Components.Dashboard (initialDashboardState, dashboardComponent)
import Humblr.Config (AppEffects)

main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- runUI dashboardComponent (parentState initialDashboardState) body
  pure unit
