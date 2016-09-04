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
import Humblr.Requests
import Halogen.HTML.Indexed as H
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode ((.?), class DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson, (~>), (:=), class EncodeJson)
import Data.Array (filter, (:))
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Humblr.Config (AppEffects, authCookieName, apiURL)
import Humblr.Requests (getWithToken, deleteWithToken)
import Network.HTTP.Affjax (AffjaxResponse, AJAX)
import Network.HTTP.StatusCode (StatusCode(..))
import Web.Storage (STORAGE, getItem, session)

type DashboardState
  = { token :: Maybe String
    , username :: Maybe String
    , posts :: Array Post
    , err :: Maybe String
    }

initialDashboardState :: DashboardState
initialDashboardState
  = { token: Nothing
  , username: Nothing
  , posts: []
  , err: Nothing
  }

data DashboardQuery a = Posts a

type ChildState = Either LoginState PostState
type ChildQuery = Coproduct LoginQuery PostQuery
type ChildSlot = Either Unit PostSlot

cpLogin :: ChildPath LoginState ChildState LoginQuery ChildQuery Unit ChildSlot
cpLogin = cpL

cpPost :: ChildPath PostState ChildState PostQuery ChildQuery PostSlot ChildSlot
cpPost = cpR

type State e
  = ParentState
      DashboardState ChildState
      DashboardQuery ChildQuery
      (Aff (AppEffects e))
      ChildSlot

type Query = Coproduct DashboardQuery (ChildF ChildSlot ChildQuery)

type DashboardHTML e
  = ParentHTML
      ChildState
      DashboardQuery ChildQuery
      (Aff (AppEffects e))
      ChildSlot

type DashboardDSL e
  = ParentDSL
      DashboardState ChildState
      DashboardQuery ChildQuery
      (Aff (AppEffects e))
      ChildSlot

renderUsername :: forall e. Maybe String -> Array (DashboardHTML e)
renderUsername Nothing = []
renderUsername (Just uname) = [H.p_ [H.text uname]]

renderError :: forall e. Maybe String -> Array (DashboardHTML e)
renderError Nothing = []
renderError (Just err) = [H.p_ [H.text $ "Error: " <> err]]

isAuthor :: Maybe String -> String -> Boolean
isAuthor Nothing _ = false
isAuthor (Just username) username' = username == username'

renderPost :: forall e. Maybe String -> Post -> DashboardHTML e
renderPost username (Post post)
  = H.slot' cpPost (PostSlot post.id) $
      \_ ->
        { component: postComponent
        , initialState:
            initialPostState
              post.id
              post.title
              post.body
              post.author
              (isAuthor username post.author)
        }

render :: forall e. DashboardState -> DashboardHTML e
render state
  = H.div_ $
      renderUsername state.username <>
      renderError state.err <>
      [H.slot' cpLogin unit $
        \_ ->
          { component: loginComponent
          , initialState: initialLoginState
          }
        , H.div_ $ map (renderPost state.username) state.posts
        ]

eval :: forall e. DashboardQuery ~> DashboardDSL e
eval (Posts next) = pure next

removePost :: PostSlot -> DashboardState -> DashboardState
removePost (PostSlot pid) state
  = state { posts = filter (\(Post p) -> p.id /= pid) state.posts }

replacePost :: Post -> Post -> Post
replacePost (Post newPost) (Post oldPost)
  = Post $ if oldPost.id == newPost.id then newPost else oldPost

peek :: forall x e. ChildF ChildSlot ChildQuery x -> DashboardDSL e Unit
peek (ChildF (Left unit) (Coproduct (Left q)))
  = case q of
      Logout _ -> modify \_ -> initialDashboardState
      Login _ _ _ -> do
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
                Right posts -> modify (_ { posts = posts })
                Left err -> modify (_ { err = Just err })
      _ -> pure unit

peek (ChildF (Right slot) (Coproduct (Right q)))
  = case q of
      Delete _ -> do
        maybeToken <- gets _.token
        response <- fromAff $ deletePost maybeToken slot
        when
          (response.status == StatusCode 200)
          (modify $ removePost slot)
      Save _ -> do
        maybeToken <- gets _.token
        maybePost <- query' cpPost slot $ request GetPost
        case maybePost of
          Nothing -> pure unit
          Just post -> do
            posts <- gets _.posts
            response <- fromAff $ updatePost maybeToken post
            when
              (response.status == StatusCode 200)
              (modify (_ { posts = map (replacePost post) posts }))
      _ -> pure unit

peek _ = pure unit

dashboardComponent :: forall e. Component (State e) Query (Aff (AppEffects e))
dashboardComponent
  = parentComponent $ { render: render, eval: eval, peek: Just peek }

getToken :: forall e. Eff (storage :: STORAGE | e) (Maybe String)
getToken = session >>= getItem authCookieName

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

getPosts :: forall e.
            String
         -> Aff (ajax :: AJAX | e) (Either String (Array Post))
getPosts token = do
  res <- getWithToken token "/my/posts"
  pure $ decodeJson res.response

deletePost :: forall e.
              Maybe String
           -> PostSlot
           -> Aff (ajax :: AJAX | e) (AffjaxResponse Json)
deletePost token (PostSlot pid)
  = deleteWithToken
      (fromMaybe "" token)
      (apiURL <> "/posts/" <> show pid <> "/delete")

newtype UpdatePost = UpdatePost { title :: String, body :: String }

instance encodeJsonUpdatePost :: EncodeJson UpdatePost where
  encodeJson (UpdatePost post) =
    "title" := post.title
    ~> "body" := post.body
    ~> jsonEmptyObject

updatePost :: forall e.
              Maybe String
           -> Post
           -> Aff (ajax :: AJAX | e) (AffjaxResponse Json)
updatePost token (Post post)
  = patchWithToken
      (fromMaybe "" token)
      (apiURL <> "/posts/" <> show post.id <> "/update")
      (encodeJson $ UpdatePost { title: post.title, body: post.body })
