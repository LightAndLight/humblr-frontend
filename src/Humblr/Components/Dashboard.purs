module Humblr.Components.Dashboard (
    DashboardState
    , DashboardQuery
    , initialDashboardState
    , dashboardComponent
) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Argonaut.Decode ((.?), class DecodeJson, decodeJson)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Generic (gEq, gCompare)
import Data.HTTP.Method (Method(..))
import Halogen
import Halogen.HTML.Indexed as H
import Network.HTTP.Affjax (AJAX, URL, Affjax, affjax, defaultRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Web.Storage (STORAGE, getItem, session)

import Humblr.Components.Login
import Humblr.Config

type DashboardState = {
    token :: Maybe String
    , username :: Maybe String
    , err :: Maybe String
    }

initialDashboardState :: DashboardState
initialDashboardState = { token: Nothing, username: Nothing, err: Nothing }

data DashboardQuery a = Posts a

type State e = ParentState DashboardState LoginState DashboardQuery LoginQuery (Aff (AppEffects e)) Unit
type Query = Coproduct DashboardQuery (ChildF Unit LoginQuery)

dashboardComponent :: forall e. Component (State e) Query (Aff (AppEffects e))
dashboardComponent = parentComponent $ { render: render, eval: eval, peek: Just peek }
  where
    renderUsername :: forall f.
                      Maybe String
                   -> Array (ParentHTML LoginState DashboardQuery LoginQuery (Aff (AppEffects f)) Unit)
    renderUsername Nothing = []
    renderUsername (Just uname) = [H.p_ [H.text uname]]

    renderError :: forall f.
                      Maybe String
                   -> Array (ParentHTML LoginState DashboardQuery LoginQuery (Aff (AppEffects f)) Unit)
    renderError Nothing = []
    renderError (Just err) = [H.p_ [H.text $ "Error: " <> err]]

    render :: forall f.
              DashboardState
           -> ParentHTML LoginState DashboardQuery LoginQuery (Aff (AppEffects f)) Unit
    render state = H.div_ $ renderUsername state.username
        <> renderError state.err
        <> [H.slot unit \_ -> { component: loginComponent, initialState: initialLoginState }]

    eval :: forall f.
            DashboardQuery
         ~> ParentDSL DashboardState LoginState DashboardQuery LoginQuery (Aff (AppEffects f)) Unit
    eval (Posts next) = pure next

    peek :: forall x f.
            ChildF Unit LoginQuery x
         -> ParentDSL DashboardState LoginState DashboardQuery LoginQuery (Aff (AppEffects f)) Unit Unit
    peek (ChildF unit q) = case q of
        Login _ _ _ -> do
            isLoggedIn <- query unit $ request IsLoggedIn
            when (fromMaybe false isLoggedIn) do
                state <- get 
                case state.token of
                    Nothing -> do
                        token' <- fromEff getToken
                        modify (_ { token = token' })
                    Just token' -> pure unit

                state <- get
                case state.token of
                    Nothing -> modify (_ { err = Just "Token disappeared" })
                    Just token' -> do
                        res <- fromAff $ getMe token'
                        case res of
                            Right (UserProfile me) -> modify (_ { username = Just me.username })
                            Left err -> modify (_ { err = Just err })
        Logout _ -> modify \_ -> initialDashboardState
        _ -> pure unit

getToken :: forall e. Eff (storage :: STORAGE | e) (Maybe String)
getToken = do
    s <- session
    getItem authCookieName s

data UserProfile = UserProfile { username :: String }

instance decodeJsonUserProfile :: DecodeJson UserProfile where
    decodeJson json = do
        obj <- decodeJson json
        username <- obj .? "username"
        pure $ UserProfile { username: username }

getT :: forall e a. Respondable a => String -> URL -> Affjax e a
getT token url = affjax defaultRequest {
    method = Left GET
    , headers = RequestHeader "auth" token : defaultRequest.headers
    , url = url }

postT :: forall e a b. (Requestable a, Respondable b) => String -> URL -> a -> Affjax e b
postT token url content = affjax defaultRequest {
    content = Just content
    , method = Left POST
    , headers = RequestHeader "auth" token : defaultRequest.headers
    , url = url }

getMe :: forall e. String -> Aff (ajax :: AJAX | e) (Either String UserProfile)
getMe token = do
    res <- getT token "/me"
    pure $ decodeJson res.response
