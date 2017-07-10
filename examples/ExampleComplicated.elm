module Main exposing (main)

import Html
import Process
import ProgramRecord
import Task
import WakeProgram


main : Program Never (ProgramRecord.AndThenModel (ProgramRecord.CacheModel ()) WakeProgram.Model) (ProgramRecord.AndThenMsg (ProgramRecord.CacheMsg (Maybe String) ()) WakeProgram.Msg)
main =
    ProgramRecord.completableProgram
        { init = Err ( (), Process.sleep 700 |> Task.perform identity )
        , update = \() () -> Ok (Just "first step done!")
        , subscriptions = \() -> Sub.none
        , view = \() -> Html.text "First..."
        }
        |> ProgramRecord.cache
            -- { read = Task.succeed (Just "READ")
            { read = Task.fail ()
            , write = Debug.log "write" >> (\_ -> Task.succeed ())
            , loadingView = Nothing
            }
        |> ProgramRecord.andThen (WakeProgram.program 700)
        |> ProgramRecord.toProgram
