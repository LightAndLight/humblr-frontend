module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen (runUI)
import Halogen.Util (awaitBody, runHalogenAff)

import Humblr.Components.Login
import Humblr.Config

main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    driver <- runUI loginComponent initialLoginState body
    pure unit
