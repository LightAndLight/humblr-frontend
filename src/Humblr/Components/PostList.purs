module Humblr.Components.PostList (
    Post(..)
    , PostListState
    , PostListQuery
    , PostListState'
    , PostListQuery'(..)
    , PostSlot(..)
    , initialPostListState
    , postListComponent
) where

import Prelude (class Ord, class Eq, type (~>), pure, bind, map, ($), (/=), (==))
import Control.Monad.Aff (Aff)
import Data.Argonaut.Decode ((.?), class DecodeJson, decodeJson)
import Data.Array (filter)
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(Nothing, Just))
import Halogen (ParentDSL, ParentHTML, Component, ChildF, ParentState, parentComponent, modify, set)
import Halogen.HTML.Indexed as H

import Humblr.Components.Post (PostQuery, PostState, initialPostState, postComponent)
import Humblr.Config (AppEffects)


newtype Post = Post { id :: Int, title :: String, body :: String, author :: String }

instance decodeJsonPost :: DecodeJson Post where
    decodeJson json = do
        obj <- decodeJson json
        pid <- obj .? "id"
        author <- obj .? "author"
        title <- obj .? "title"
        body <- obj .? "body"
        pure $ Post {
            id: pid
            , author: author
            , title: title
            , body: body
            }

type PostListState' = { username :: Maybe String, posts :: Array Post }

initialPostListState :: PostListState'
initialPostListState = { username: Nothing, posts: [] }

data PostListQuery' a = Load (Maybe String) (Array Post) a
                      | RemovePost PostSlot a
                      | Clear a

newtype PostSlot = PostSlot Int
derive instance eqPostSlot :: Eq PostSlot
derive instance ordPostSlot :: Ord PostSlot

type PostListState e = ParentState PostListState' PostState PostListQuery' PostQuery (Aff (AppEffects e)) PostSlot
type PostListQuery = Coproduct PostListQuery' (ChildF PostSlot PostQuery)

postListComponent :: forall e. Component (PostListState e) PostListQuery (Aff (AppEffects e))
postListComponent = parentComponent $ { render: render, eval: eval, peek: Nothing }
  where
    isAuthor :: Maybe String -> String -> Boolean
    isAuthor Nothing _ = false
    isAuthor (Just username) username' = username == username'

    renderPost :: forall f.
                  Maybe String
               -> Post
               -> ParentHTML PostState PostListQuery' PostQuery (Aff (AppEffects f)) PostSlot
    renderPost username (Post post) = H.slot (PostSlot post.id) \_ -> {
        component: postComponent
        , initialState: initialPostState post.id post.title post.body post.author (isAuthor username post.author)
        }
    
    render :: forall f.
              PostListState'
           -> ParentHTML PostState PostListQuery' PostQuery (Aff (AppEffects f)) PostSlot
    render state = H.div_ $ map (renderPost state.username) state.posts

    removePost :: PostSlot -> PostListState' -> PostListState'
    removePost (PostSlot pid) state = state { posts = filter (\(Post p) -> p.id /= pid) state.posts }

    eval :: forall f.
            PostListQuery'
         ~> ParentDSL PostListState' PostState PostListQuery' PostQuery (Aff (AppEffects f)) PostSlot
    eval (Load username posts next) = do
        modify (_ { username = username, posts = posts }) 
        pure next
    eval (Clear next) = do
        set initialPostListState
        pure next
    eval (RemovePost slot next) = do
        modify $ removePost slot
        pure next
