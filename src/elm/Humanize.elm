module Humanize exposing (..)

{-| -}


{-| -}
pluralize : String -> String -> Int -> String
pluralize singular plural count =
    if count == 1 then
        singular
    else
        plural


onceTwiceThrice : Int -> Maybe String
onceTwiceThrice count =
    if count == 1 then
        Just "once"
    else if count == 2 then
        Just "twice"
    else if count == 3 then
        Just "thrice"
    else
        Nothing
