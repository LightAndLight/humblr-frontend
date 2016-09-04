module Humblr.Components.Post (
  Post(..)
  , PostState
  , PostQuery(..)
  , PostSlot(..)
  , initialPostState
  , postComponent
) where

import Prelude
import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as HP
import Data.Argonaut.Decode ((.?), decodeJson, class DecodeJson)

newtype Post
  = Post { id :: Int, title :: String, body :: String, author :: String }

instance decodeJsonPost :: DecodeJson Post where
  decodeJson json = do
    obj <- decodeJson json
    pid <- obj .? "id"
    author <- obj .? "author"
    title <- obj .? "title"
    body <- obj .? "body"
    pure $ Post { id: pid, author: author, title: title, body: body }

newtype PostSlot = PostSlot Int
derive instance eqPostSlot :: Eq PostSlot
derive instance ordPostSlot :: Ord PostSlot

type PostState
  = { id :: Int
    , title :: String
    , body :: String
    , author :: String
    , editable :: Boolean
    , editing :: Boolean
    , markedForDelete :: Boolean
    }

initialPostState :: Int -> String -> String -> String -> Boolean -> PostState
initialPostState pid title body author editable
  = { id: pid
    , title: title
    , body: body
    , author: author
    , editable: editable
    , editing: false
    , markedForDelete: false
    }

data PostQuery a
  = Edit a
  | Save a
  | SetBody String a
  | SetTitle String a
  | MarkDelete a
  | UnmarkDelete a
  | Delete a
  | GetPost (Post -> a)

renderDeleteConfirm :: PostState -> ComponentHTML PostQuery
renderDeleteConfirm state
  | state.markedForDelete
    = H.div_
        [ H.p_ [H.text "Confirm delete"]
        , H.button [E.onClick (E.input_ Delete)] [H.text "Yes"]
        , H.button [E.onClick (E.input_ UnmarkDelete)] [H.text "No"]
        ]
  | otherwise = H.button [E.onClick (E.input_ MarkDelete)] [H.text "Delete"]

renderUpdatePost :: PostState -> Array (ComponentHTML PostQuery)
renderUpdatePost state
  | state.editable
      = [ H.button [E.onClick (E.input_ Edit)] [H.text "Edit"]
        , renderDeleteConfirm state
        ]
  | otherwise = []

render :: PostState -> ComponentHTML PostQuery
render state
  | state.editing
    = H.div_ 
        [ H.div_
            [ H.input
                [ HP.inputType HP.InputText
                , HP.value state.title
                , E.onValueChange (E.input SetTitle)
                ]
            ]
        , H.div_
            [ H.textarea
                [ HP.value state.body
                , E.onValueChange (E.input SetBody)
                ]
            ]
        , H.div_ [H.button [E.onClick (E.input_ Save)] [H.text "Save"]]
        ]
  | otherwise
    = H.div_ $
        [ H.h1_ [H.text state.title]
        , H.p_ [H.text state.author]
        , H.p_ [H.text state.body]
        ] <> renderUpdatePost state

eval :: forall g. PostQuery ~> ComponentDSL PostState PostQuery g
eval (Edit next) = do
  editable <- gets _.editable
  when editable $ modify (_ { editing = true })
  pure next

eval (Save next) = do
  editable <- gets _.editable
  when editable $ modify (_ { editing = false })
  pure next

eval (SetTitle title next) = modify (_ { title = title }) $> next
eval (SetBody body next) = modify (_ { body = body }) $> next
eval (MarkDelete next) = modify (_ { markedForDelete = true }) $> next
eval (UnmarkDelete next) = modify (_ { markedForDelete = false }) $> next
eval (Delete next) = pure next
eval (GetPost next) = do
  state <- get
  pure <<< next $
    Post
      { id: state.id
      , author: state.author
      , title: state.title
      , body: state.body
      }

postComponent :: forall e. Component PostState PostQuery e
postComponent = component { render, eval }
