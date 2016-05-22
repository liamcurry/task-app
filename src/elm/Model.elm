module Model exposing (..)

-- LIB

import Json.Decode as D exposing ((:=))
import Json.Encode as E
import Time


-- INTERNAL

import Component.HabitForm as HabitForm
import Component.HabitList as HabitList
import Component.Settings as Settings
import Util


-- MODEL


type alias Model =
    { settings : Settings.Model
    , habitForm : HabitForm.Model
    , habitList : HabitList.Model
    , now : Time.Time
    , expanded : Maybe Expandable
    }


empty : Model
empty =
    { settings = Settings.empty
    , habitForm = HabitForm.empty
    , habitList = HabitList.empty
    , now = 0
    , expanded = Nothing
    }


decoder : D.Decoder Model
decoder =
    D.object5 Model
        ("settings" := Settings.decoder)
        ("habitForm" := HabitForm.decoder)
        ("habitList" := HabitList.decoder)
        ("now" := D.float)
        (D.maybe ("expanded" := expandableDecoder))


encode : Model -> E.Value
encode model =
    E.object
        [ ( "settings", Settings.encode model.settings )
        , ( "habitForm", HabitForm.encode model.habitForm )
        , ( "habitList", HabitList.encode model.habitList )
        , ( "now", E.float model.now )
        , ( "expanded", Util.encodeMaybe encodeExpandable model.expanded )
        ]



-- EXPANDABLE


type Expandable
    = Settings
    | HabitForm


expandableFromString : String -> Result String Expandable
expandableFromString str =
    case str of
        "Settings" ->
            Ok Settings

        "HabitForm" ->
            Ok HabitForm

        _ ->
            Err <| "'" ++ str ++ "' is not an expandable type"


expandableDecoder : D.Decoder Expandable
expandableDecoder =
    D.customDecoder D.string expandableFromString


encodeExpandable : Expandable -> E.Value
encodeExpandable expandable =
    E.string <| toString expandable
