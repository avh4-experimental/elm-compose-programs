module Main exposing (main)

import Html
import Process
import ProgramRecord
import Task


type alias Flags =
    Maybe String


type alias Model =
    Maybe String


type alias Msg =
    Maybe String


main : Program Flags Model Msg
main =
    ProgramRecord.htmlProgramWithFlags
        { init =
            \flags ->
                ( flags
                , Process.sleep 2000
                    |> Task.map (\() -> Just "Wake!")
                    |> Task.perform identity
                )
        , update = \msg model -> ( msg, Cmd.none )
        , subscriptions = \model -> Sub.none
        , view = toString >> Html.text
        }
        |> ProgramRecord.toProgramWithFlags
