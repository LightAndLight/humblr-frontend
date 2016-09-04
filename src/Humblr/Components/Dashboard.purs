module Humblr.Components.Dashboard (
    DashboardState
    , DashboardQuery
    , initialDashboardState
    , dashboardComponent
) where

import Prelude
import Halogen
import Humblr.Components.Login
import Humblr.Components.Post
import Humblr.Components.PostList
import Halogen.HTML.Indexed as H
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode ((.?), class DecodeJson, decodeJson)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct(..), coproduct)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Humblr.Config (AppEffects, authCookieName, apiURL)
import Humblr.Requests (getWithToken, deleteWithToken)
import Network.HTTP.Affjax (AffjaxResponse, AJAX)
import Network.HTTP.StatusCode (StatusCode(..))
import Web.Storage (STORAGE, getItem, session)

type DashboardState = {
    token :: Maybe String
    , username :: Maybe String
    , err :: Maybe String
    }

initialDashboardState :: DashboardState
initialDashboardState = { token: Nothing, username: Nothing, err: Nothing }

data DashboardQuery a = Posts a

type ChildState e = Either LoginState (PostListState e)
type ChildQuery = Coproduct LoginQuery PostListQuery
type ChildSlot = Either Unit Unit

cpLogin :: forall e. ChildPath LoginState (ChildState e) LoginQuery ChildQuery Unit ChildSlot
cpLogin = cpL

cpPostList :: forall e. ChildPath (PostListState e) (ChildState e) PostListQuery ChildQuery Unit ChildSlot
cpPostList = cpR

type State e = ParentState DashboardState (ChildState e) DashboardQuery ChildQuery (Aff (AppEffects e)) ChildSlot
type Query = Coproduct DashboardQuery (ChildF ChildSlot ChildQuery)

dashboardComponent :: forall e. Component (State e) Query (Aff (AppEffects e))
dashboardComponent = parentComponent $ { render: render, eval: eval, peek: Just peek }
  where
    renderUsername :: forall f.
                      Maybe String
                   -> Array (ParentHTML (ChildState f) DashboardQuery ChildQuery (Aff (AppEffects f)) ChildSlot)
    renderUsername Nothing = []
    renderUsername (Just uname) = [H.p_ [H.text uname]]

    renderError :: forall f.
                      Maybe String
                   -> Array (ParentHTML (ChildState f) DashboardQuery ChildQuery (Aff (AppEffects f)) ChildSlot)
    renderError Nothing = []
    renderError (Just err) = [H.p_ [H.text $ "Error: " <> err]]

    render :: forall f.
              DashboardState
           -> ParentHTML (ChildState f) DashboardQuery ChildQuery (Aff (AppEffects f)) ChildSlot
    render state = H.div_ $ renderUsername state.username
        <> renderError state.err
        <> [H.slot' cpLogin unit \_ -> { component: loginComponent, initialState: initialLoginState }]
        <> [H.slot' cpPostList unit \_ -> { component: postListComponent, initialState: parentState initialPostListState }]

    eval :: forall f.
            DashboardQuery
         ~> ParentDSL DashboardState (ChildState f) DashboardQuery ChildQuery (Aff (AppEffects f)) ChildSlot
    eval (Posts next) = pure next

    peekLogin :: forall f a.
                 LoginQuery a
              -> ParentDSL DashboardState (ChildState f) DashboardQuery ChildQuery (Aff (AppEffects f)) ChildSlot Unit
    peekLogin (Login _ _ _) = do
        isLoggedIn <- query' cpLogin unit $ request IsLoggedIn
        when (fromMaybe false isLoggedIn) do
            state <- get 
            case state.token of
                Nothing -> do
                    token' <- fromEff getToken
                    modify (_ { token = token' })
                Just token' -> pure unit

            state' <- get
            case state'.token of
                Nothing -> modify (_ { err = Just "Token disappeared" })
                Just token' -> do
                    res <- fromAff $ getMe token'
                    case res of
                        Right (UserProfile me) -> modify (_ { username = Just me.username })
                        Left err -> modify (_ { err = Just err })
                    res' <- fromAff $ getPosts token'
                    username <- gets _.username
                    case res' of
                        Right posts -> do
                            query' cpPostList unit $ action (Coproduct <<< Left <<< Load username posts)
                            pure unit
                        Left err -> modify (_ { err = Just err })
    peekLogin (Logout _) = do
        query' cpPostList unit $ action (Coproduct <<< Left <<< Clear)
        modify \_ -> initialDashboardState
    peekLogin _ = pure unit

    peekPost :: forall f x.
                ChildF PostSlot PostQuery x
             -> ParentDSL DashboardState (ChildState f) DashboardQuery ChildQuery (Aff (AppEffects f)) ChildSlot Unit
    peekPost (ChildF slot q) = case q of
        Delete _ -> do
            maybeToken <- gets _.token
            response <- fromAff $ deletePost maybeToken slot
            if (response.status == StatusCode 200)
                then do
                    query' cpPostList unit (action (Coproduct <<< Left <<< RemovePost slot))
                    pure unit
                else pure unit
        _ -> pure unit

    peekPostList :: forall x f.
                    PostListQuery x
                 -> ParentDSL DashboardState (ChildState f) DashboardQuery ChildQuery (Aff (AppEffects f)) ChildSlot Unit
    peekPostList = coproduct (const $ pure unit) peekPost

    peek :: forall x f.
            ChildF ChildSlot ChildQuery x
         -> ParentDSL DashboardState (ChildState f) DashboardQuery ChildQuery (Aff (AppEffects f)) ChildSlot Unit
    peek (ChildF _ q) = coproduct peekLogin peekPostList q

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

getMe :: forall e. String -> Aff (ajax :: AJAX | e) (Either String UserProfile)
getMe token = do
    res <- getWithToken token "/me"
    pure $ decodeJson res.response

getPosts :: forall e. String -> Aff (ajax :: AJAX | e) (Either String (Array Post))
getPosts token = do
    res <- getWithToken token "/my/posts"
    pure $ decodeJson res.response

deletePost :: forall e.
              Maybe String
           -> PostSlot
           -> Aff (ajax :: AJAX | e) (AffjaxResponse Json)
deletePost token (PostSlot pid) =
  deleteWithToken (fromMaybe "" token) (apiURL <> "/post/" <> show pid <> "/delete")
