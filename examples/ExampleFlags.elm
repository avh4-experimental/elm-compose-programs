module Main exposing (main)

import ProgramRecord
import WakeProgram


main : Program (Maybe String) WakeProgram.Model WakeProgram.Msg
main =
    WakeProgram.program 700
        |> ProgramRecord.toProgramWithFlags
