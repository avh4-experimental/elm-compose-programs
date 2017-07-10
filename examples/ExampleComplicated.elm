module Main exposing (main)

import Html
import Process
import ProgramRecord
import Task
import WakeProgram


main : Program Never (ProgramRecord.AndThenModel () WakeProgram.Model) (ProgramRecord.AndThenMsg () WakeProgram.Msg)
main =
    ProgramRecord.completableProgram
        { init = Err ( (), Process.sleep 700 |> Task.perform identity )
        , update = \() () -> Ok (Just "first step done!")
        , subscriptions = \() -> Sub.none
        , view = \() -> Html.text "First..."
        }
        |> ProgramRecord.andThen (WakeProgram.program 700)
        |> ProgramRecord.toProgram
