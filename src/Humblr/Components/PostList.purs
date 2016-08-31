module Humblr.Components.PostList (
    Post(..)
    , PostListState
    , PostListQuery
    , PostListState'
    , PostListQuery'(..)
    , PostSlot
    , initialPostListState
    , postListComponent
) where

import Prelude
import Data.Argonaut.Decode ((.?), class DecodeJson, decodeJson)
import Data.Array (filter)
import Data.Functor.Coproduct (Coproduct)
import Data.Generic (gEq, gCompare)
import Data.Maybe (Maybe(..))
import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as E

import Humblr.Components.Post

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

newtype PostSlot = PostSlot Int
derive instance eqPostSlot :: Eq PostSlot
derive instance ordPostSlot :: Ord PostSlot

type PostListState g = ParentState PostListState' PostState PostListQuery' PostQuery g PostSlot
type PostListQuery = Coproduct PostListQuery' (ChildF PostSlot PostQuery)

postListComponent :: forall g. Functor g => Component (PostListState g) PostListQuery g
postListComponent = parentComponent $ { render: render, eval: eval, peek: Just peek }
  where
    isAuthor :: Maybe String -> String -> Boolean
    isAuthor Nothing _ = false
    isAuthor (Just username) username' = username == username'

    renderPost :: forall f.
                  Maybe String
               -> Post
               -> ParentHTML PostState PostListQuery' PostQuery g PostSlot
    renderPost username (Post post) = H.slot (PostSlot post.id) \_ -> {
        component: postComponent
        , initialState: initialPostState post.id post.title post.body post.author (isAuthor username post.author)
        }
    
    render :: forall f.
              PostListState'
           -> ParentHTML PostState PostListQuery' PostQuery g PostSlot
    render state = H.div_ $ map (renderPost state.username) state.posts

    eval :: forall f.
            PostListQuery'
         ~> ParentDSL PostListState' PostState PostListQuery' PostQuery g PostSlot
    eval (Load username posts next) = do
        modify (_ { username = username, posts = posts }) 
        pure next

    removePost :: PostSlot -> PostListState' -> PostListState'
    removePost (PostSlot pid) state = state { posts = filter (\(Post p) -> p.id /= pid) state.posts }

    peek :: forall f x.
            ChildF PostSlot PostQuery x
         -> ParentDSL PostListState' PostState PostListQuery' PostQuery g PostSlot Unit
    peek (ChildF slot query) = case query of
        Delete _ -> do
            posts <- gets _.posts
            modify $ removePost slot
        _        -> pure unit
