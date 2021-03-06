module Humblr.Components.Login
  ( LoginQuery(..)
  , LoginState
  , initialLoginState
  , loginComponent
  ) where

import Prelude (type (~>), Unit, bind, otherwise, pure, ($), ($>), (<<<), (<>), (>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, try, message)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?), (.??))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (Component, ComponentDSL, ComponentHTML, component, fromAff, fromEff, gets, modify)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed (ButtonType(..), InputType(..), buttonType, inputType, name, value)
import Network.HTTP.Affjax (AJAX, post)
import Web.Storage (STORAGE, removeItem, session, setItem)

import Humblr.Config (AppEffects, authCookieName, apiURL)

newtype LoginUser = LoginUser { username :: String, password :: String }

instance encodeJsonLoginUser :: EncodeJson LoginUser where
  encodeJson (LoginUser user)
    = "username" := user.username
   ~> "password" := user.password
   ~> jsonEmptyObject

newtype LoginResponse = LoginResponse (Either String String)

instance decodeJsonEitherErrorToken :: DecodeJson LoginResponse where
  decodeJson json = do
    obj <- decodeJson json
    token <- obj .?? "token"
    case token of
      Just tok -> pure <<< LoginResponse $ Right tok
      Nothing -> do
        err <- obj .? "error"
        pure <<< LoginResponse $ Left err

type LoginState
  = { username :: String
    , password :: String
    , message :: String
    , loggedIn :: Boolean
    }

data LoginQuery a
  = SetUsername String a
  | SetPassword String a
  | Login String String a
  | IsLoggedIn (Boolean -> a)
  | Logout a

render :: LoginState -> ComponentHTML LoginQuery
render state
  | state.loggedIn
    = H.div_
        [ H.button
            [ buttonType ButtonSubmit
            , E.onClick (E.input_ Logout)
            ] [H.text "Log out"]
        ]
  | otherwise
    = H.div_
        [ H.input
            [ inputType InputText
            , name "username"
            , E.onValueInput (E.input SetUsername)
            ]
        , H.input
            [ inputType InputPassword
            , name "password"
            , E.onValueInput (E.input SetPassword)
            ]
        , H.input
            [ inputType InputSubmit
            , value "Log in"
            , E.onClick (E.input_ $ Login state.username state.password)
            ]
        , H.p_ [H.text state.message]
        ]

eval :: forall eff.
        LoginQuery
     ~> ComponentDSL LoginState LoginQuery (Aff (AppEffects eff))

eval (SetUsername username' next)
  = modify (_ { username = username', message = "" }) $> next

eval (SetPassword password' next)
  = modify (_ { password = password', message = "" }) $> next

eval (Login username password next) = do
  res <- fromAff <<< doLogin $
    LoginUser { username: username, password: password }
  case res of
    Left err -> modify (_ { message = err })
    Right token -> do
      res' <- fromEff (saveSession false token)
      case res' of
        Left err -> modify (_ { message = message err })
        Right _ ->
          modify $
            _ { username = ""
              , password = ""
              , message = ""
              , loggedIn = true
              }
  pure next

eval (IsLoggedIn next) = do
  isLoggedIn <- gets _.loggedIn
  pure $ next isLoggedIn

eval (Logout next) = do
  fromEff doLogout
  modify (_ { loggedIn = false })
  pure next

loginComponent :: forall e. Component LoginState LoginQuery (Aff (AppEffects e))
loginComponent = component { render, eval }

saveSession :: forall e.
               Boolean
            -> String
            -> Eff (storage :: STORAGE | e) (Either Error Unit)

saveSession b token = do
  s <- session
  try $ setItem authCookieName token s

doLogin :: forall e. LoginUser -> Aff (ajax :: AJAX | e) (Either String String)
doLogin user = do
  result <- post (apiURL <> "/login") (encodeJson $ user)
  pure $ case decodeJson result.response of
    Right (LoginResponse e) -> e
    Left err -> Left err

doLogout :: forall e. Eff (storage :: STORAGE | e) Unit
doLogout = session >>= removeItem authCookieName

initialLoginState :: LoginState
initialLoginState = { username: "", password: "", message: "", loggedIn: false }
