module Humblr.Requests (
  getWithToken
  , postWithToken
  , deleteWithToken
  , patchWithToken
) where

import Prelude (($))
import Data.Array ((:))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (defaultRequest, Affjax, URL, affjax)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))

getWithToken :: forall e a. Respondable a => String -> URL -> Affjax e a
getWithToken token url = affjax defaultRequest {
  method = Left GET
  , headers = RequestHeader "auth" token : defaultRequest.headers
  , url = url
  }

deleteWithToken :: forall e a. Respondable a => String -> URL -> Affjax e a
deleteWithToken token url = affjax defaultRequest {
  method = Left DELETE
  , headers = RequestHeader "auth" token : defaultRequest.headers
  , url = url
  }

postWithToken :: forall e a b. (Requestable a, Respondable b) => String -> URL -> a -> Affjax e b
postWithToken token url content = affjax defaultRequest {
  content = Just content
  , method = Left POST
  , headers = RequestHeader "auth" token : defaultRequest.headers
  , url = url
  }

patchWithToken :: forall e a b. (Requestable a, Respondable b) => String -> URL -> a -> Affjax e b
patchWithToken token url content = affjax defaultRequest {
  content = Just content
  , method = Left PATCH
  , headers = RequestHeader "auth" token : defaultRequest.headers
  , url = url
  }
