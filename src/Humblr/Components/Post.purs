module Humblr.Components.Post (
    PostState
    , PostQuery(..)
    , initialPostState
    , postComponent
) where

import Prelude
import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as E

import Humblr.Config

type PostState = {
    id :: Int
    , title :: String
    , body :: String
    , author :: String
    , editable :: Boolean
    , editing :: Boolean
    , markedForDelete :: Boolean
    }

initialPostState :: Int -> String -> String -> String -> Boolean -> PostState
initialPostState pid title body author editable = {
    id: pid
    , title: title
    , body: body
    , author: author
    , editable: editable
    , editing: false
    , markedForDelete: false
    }

data PostQuery a = Edit a
                 | Save a
                 | SetBody String a
                 | SetTitle String a
                 | MarkDelete a
                 | UnmarkDelete a
                 | Delete a

postComponent :: forall e. Component PostState PostQuery e
postComponent = component { render, eval }
  where
    renderDeleteConfirm :: PostState -> ComponentHTML PostQuery
    renderDeleteConfirm state
      | state.markedForDelete = H.div_ [
            H.p_ [H.text "Confirm delete"]
            , H.button [HP.value "Yes", E.onClick (E.input_ Delete)] []
            , H.button [HP.value "No", E.onClick (E.input_ UnmarkDelete)] []
            ]
      | otherwise = H.button [HP.value "Delete", E.onClick (E.input_ MarkDelete)] []

    renderUpdatePost :: PostState -> Array (ComponentHTML PostQuery)
    renderUpdatePost state
      | state.editable = [
            H.button [HP.value "Edit", E.onClick (E.input_ Edit)] []
            , renderDeleteConfirm state
            ]
      | otherwise = []

    render :: PostState -> ComponentHTML PostQuery
    render state
      | state.editing = H.div_ [
            H.input [HP.inputType HP.InputText, HP.value state.title, E.onValueChange (E.input SetTitle)]
            , H.textarea [HP.value state.body, E.onValueChange (E.input SetBody)]
            , H.button [HP.value "Save", E.onClick (E.input_ Save)] []
            ]
      | otherwise = H.div_ $ [
            H.h1_ [H.text state.title]
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
        when editable do
            -- send to db
            modify (_ { editing = false })
        pure next
    eval (SetTitle title next) = do
        modify (_ { title = title })
        pure next
    eval (SetBody body next) = do
        modify (_ { body = body })
        pure next
    eval (MarkDelete next) = do
        modify (_ { markedForDelete = true })
        pure next
    eval (UnmarkDelete next) = do
        modify (_ { markedForDelete = false })
        pure next
    eval (Delete next) = do
        marked <- gets _.markedForDelete
        when marked do
            -- delete from DB
            pure unit
        pure next
