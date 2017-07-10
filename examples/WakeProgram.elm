module WakeProgram exposing (Model, Msg, program)

import Html
import Process
import ProgramRecord exposing (ProgramRecord)
import Task
import Time exposing (Time)


type alias Model =
    Maybe String


type alias Msg =
    Maybe String


program : Time -> ProgramRecord (Maybe String) Never Model Msg
program delay =
    ProgramRecord.htmlProgramWithFlags
        { init =
            \init ->
                ( init
                , Process.sleep delay
                    |> Task.map (\() -> Just "Wake!")
                    |> Task.perform identity
                )
        , update = \msg model -> ( msg, Cmd.none )
        , subscriptions = \model -> Sub.none
        , view = toString >> Html.text
        }
