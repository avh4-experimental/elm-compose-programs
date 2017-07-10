module Main exposing (main)

import ProgramRecord
import WakeProgram


main : Program Never WakeProgram.Model WakeProgram.Msg
main =
    WakeProgram.program 700
        |> ProgramRecord.applyFlags Nothing
        |> ProgramRecord.toProgram
