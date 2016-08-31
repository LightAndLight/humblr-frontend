module Humblr.Components.PostList (
    Post
    , PostListState
    , PostListQuery
    , PostListState'
    , PostListQuery'
    , PostSlot
    , initialPostListState
    , postListComponent
) where

import Prelude
import Data.Array (filter)
import Data.Functor.Coproduct (Coproduct)
import Data.Generic (gEq, gCompare)
import Data.Maybe (Maybe(..))
import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as E

import Humblr.Components.Post

type Post = { id :: Int, title :: String, body :: String, author :: String }

type PostListState' = { posts :: Array Post }

initialPostListState :: PostListState'
initialPostListState = { posts: [] }

data PostListQuery' a = Load (Array Post) a

newtype PostSlot = PostSlot Int
derive instance eqPostSlot :: Eq PostSlot
derive instance ordPostSlot :: Ord PostSlot

type PostListState g = ParentState PostListState' PostState PostListQuery' PostQuery g PostSlot
type PostListQuery = Coproduct PostListQuery' (ChildF PostSlot PostQuery)

postListComponent :: forall g. Functor g => Component (PostListState g) PostListQuery g
postListComponent = parentComponent $ { render: render, eval: eval, peek: Just peek }
  where
    renderPost :: forall f.
                  Post
               -> ParentHTML PostState PostListQuery' PostQuery g PostSlot
    renderPost post = H.slot (PostSlot post.id) \_ -> {
        component: postComponent
        , initialState: initialPostState post.id post.title post.body post.author
        }
    
    render :: forall f.
              PostListState'
           -> ParentHTML PostState PostListQuery' PostQuery g PostSlot
    render state = H.div_ $ map renderPost state.posts

    eval :: forall f.
            PostListQuery'
         ~> ParentDSL PostListState' PostState PostListQuery' PostQuery g PostSlot
    eval (Load posts next) = do
        modify (_ { posts = posts }) 
        pure next

    removePost :: PostSlot -> PostListState' -> PostListState'
    removePost (PostSlot pid) state = state { posts = filter (\p -> p.id /= pid) state.posts }

    peek :: forall f x.
            ChildF PostSlot PostQuery x
         -> ParentDSL PostListState' PostState PostListQuery' PostQuery g PostSlot Unit
    peek (ChildF slot query) = case query of
        Delete _ -> do
            posts <- gets _.posts
            modify $ removePost slot
        _        -> pure unit
