module Main exposing (main)

import Html
import Process
import ProgramRecord
import Task


type alias Model =
    Maybe String


type alias Msg =
    Maybe String


main : Program Never Model Msg
main =
    ProgramRecord.htmlProgram
        { init =
            ( Nothing
            , Process.sleep 700
                |> Task.map (\() -> Just "Wake!")
                |> Task.perform identity
            )
        , update = \msg model -> ( msg, Cmd.none )
        , subscriptions = \model -> Sub.none
        , view = toString >> Html.text
        }
        |> ProgramRecord.toProgram
