module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed
import Halogen.Util (awaitBody, runHalogenAff)

apiURL :: String
apiURL = "https://127.0.0.1:3000"

data LoginQuery a = LoginQuery a

loginForm :: forall g. Component { loggedIn :: Boolean } LoginQuery g
loginForm = component { render, eval }
  where
    render state
      | not state.loggedIn = H.div_ [
            H.form_ [
                H.input [inputType InputText, name "username"] 
                , H.input [inputType InputPassword, name "password"] 
                , H.input [inputType InputSubmit, value "Log in"] 
            ]
        ]
      | otherwise = H.div_ [
            H.button [value "Log out"] [] 
        ]
    
    eval :: LoginQuery ~> ComponentDSL { loggedIn :: Boolean } LoginQuery g
    eval (LoginQuery a) = pure a

main :: forall e. Eff (HalogenEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    driver <- runUI loginForm { loggedIn: false } body
    pure unit
