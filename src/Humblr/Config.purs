module Humblr.Config where

import Halogen (HalogenEffects)
import Network.HTTP.Affjax (AJAX)
import Web.Storage (STORAGE)

type AppEffects e = HalogenEffects (ajax :: AJAX, storage :: STORAGE | e)

authCookieName :: String
authCookieName = "humblr-auth-cookie"

apiURL :: String
apiURL = "https://127.0.0.1:3000"
