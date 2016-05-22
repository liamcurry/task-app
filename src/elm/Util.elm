module Util exposing (encodeMaybe)

import Json.Encode as E


encodeMaybe : (a -> E.Value) -> Maybe a -> E.Value
encodeMaybe toValue maybe =
    case maybe of
        Just x ->
            toValue x

        Nothing ->
            E.null
