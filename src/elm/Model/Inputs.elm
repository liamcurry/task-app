module Model.Inputs exposing (..)

-- LIB

import Css
import Dict
import Json.Decode as D
import Json.Encode as E
import String


-- INTERNAL

import Style


type alias Inputs =
    Dict.Dict String String


empty : Inputs
empty =
    Dict.empty


decoder : D.Decoder Inputs
decoder =
    D.dict D.string


encode : Inputs -> E.Value
encode inputs =
    E.object <| List.map (\( k, v ) -> ( k, E.string v )) <| Dict.toList inputs


get : String -> Inputs -> Maybe String
get =
    Dict.get


insert : String -> String -> Inputs -> Dict.Dict String String
insert =
    Dict.insert



-- INPUTS


color : Css.Color -> String -> Css.Color
color =
    withFallback Style.hexToColor


int : Int -> String -> Int
int =
    withFallback String.toInt


float : Float -> String -> Float
float =
    withFallback String.toFloat



-- HELPERS


withFallback : (String -> Result String a) -> a -> String -> a
withFallback toWhatever fallback str =
    str
        |> toWhatever
        |> Result.toMaybe
        |> Maybe.withDefault fallback
