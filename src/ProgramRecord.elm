module ProgramRecord
    exposing
        ( ProgramRecord
        , ProgramType(..)
        , applyInit
        , completableProgram
        , getLocationChange
        , htmlProgram
        , htmlProgramWithFlags
        , navigationProgram
        , navigationProgramWithFlags
        , toProgram
        , toProgramWithFlags
        , withFlags
        )

{-|


## Basics

@docs ProgramRecord, toProgram, toProgramWithFlags


## Typical programs


### elm-lang/html

@docs htmlProgram, htmlProgramWithFlags


### elm-lang/navigation

@docs navigationProgram, navigationProgramWithFlags


## Special programs

@docs completableProgram


## TODO

@docs ProgramType, applyInit, getLocationChange , withFlags

-}

import Html exposing (Html)
import Navigation exposing (Location)


{-| `update` and `init` return a `Result` where:

  - `Ok` means auth is finished; the provided value will be used to initialize the main program
  - `Err` means auth did not finish; the provided value is the new model and Cmd for the auth program

-}
type alias ProgramRecord flags done model msg =
    ProgramRecord_ flags (Result ( model, Cmd msg ) done) done model msg


type alias ProgramWithFlags flags done model msg =
    ProgramRecord_ flags (flags -> Result ( model, Cmd msg ) done) done model msg


type alias ProgramRecord_ flags init done model msg =
    { init : ProgramType flags init msg
    , update : msg -> model -> Result ( model, Cmd msg ) done
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


{-| -}
type ProgramType flags init msg
    = NoArgs init
    | WithFlags (flags -> init)
    | WithLocation (Location -> init) (Location -> msg)
    | WithBoth (flags -> Location -> init) (Location -> msg)


{-| -}
withFlags :
    ProgramWithFlags flags done model msg
    -> ProgramRecord flags done model msg
withFlags r =
    let
        fixInit :
            ProgramType flags (flags -> Result ( model, Cmd msg ) done) msg
            -> ProgramType flags (Result ( model, Cmd msg ) done) msg
        fixInit i =
            case i of
                NoArgs f ->
                    WithFlags f

                WithFlags f ->
                    WithFlags <| \flags -> f flags flags

                WithLocation f onLoc ->
                    WithBoth (flip f) onLoc

                WithBoth f onLoc ->
                    WithBoth (\flags loc -> f flags loc flags) onLoc
    in
    { init = fixInit r.init
    , update = r.update
    , subscriptions = r.subscriptions
    , view = r.view
    }


{-| -}
applyInit : ProgramType flags init msg -> Maybe flags -> Location -> init
applyInit init flags location =
    case ( flags, init ) of
        ( Nothing, NoArgs value ) ->
            value

        ( Nothing, WithLocation f _ ) ->
            f location

        ( Nothing, _ ) ->
            Debug.crash "Program requires flags, but none were provided"

        ( Just fl, WithFlags f ) ->
            f fl

        ( Just fl, WithBoth f _ ) ->
            f fl location

        ( Just _, _ ) ->
            Debug.crash "Program has flags=Never, but flags were provided"


{-| -}
getLocationChange : ProgramType flags init msg -> Maybe (Location -> msg)
getLocationChange init =
    case init of
        NoArgs _ ->
            Nothing

        WithFlags _ ->
            Nothing

        WithLocation _ f ->
            Just f

        WithBoth _ f ->
            Just f


{-| Turns a `ProgramRecord` into a real `elm-lang/core: Platform.Program`.

If your program has flags, you will need to use [`toProgramWithFlags`](#toProgramWithFlags) instead.

-}
toProgram : ProgramRecord Never Never model msg -> Platform.Program Never model msg
toProgram record =
    case record.init of
        NoArgs value ->
            Html.program
                { init = value |> handleNever
                , update = record.update >>> handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }

        WithFlags init ->
            -- TODO: should be safe to crash here
            Html.programWithFlags
                { init = init >> handleNever
                , update = record.update >>> handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }

        WithLocation init onLocationChange ->
            Navigation.program
                onLocationChange
                { init = init >> handleNever
                , update = record.update >>> handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }

        WithBoth init onLocationChange ->
            -- TODO: should be safe to crash here
            Navigation.programWithFlags
                onLocationChange
                { init = init >>> handleNever
                , update = record.update >>> handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }


{-| Turns a `ProgramRecord` into a real `elm-lang/core: Platform.Program`.

If your `flags` type is `Never`, you must use [`toProgram`](#toProgram) instead (if you don't you will get a runtime exception).

-}
toProgramWithFlags : ProgramRecord flags Never model msg -> Platform.Program flags model msg
toProgramWithFlags record =
    case record.init of
        NoArgs value ->
            -- NOTE: this is actually safe, since the ProgramRecord is NoArgs, then the `flags` type parameter is unbound, meaning it could safely be `Never`.  However, there is no `Platform.Program.map`, so we can't actually create a `Platform.Program` that won't crash at runtime, so instead we just crash now.
            Debug.crash "You are using ProgramRecord.toProgramWithFlags with a ProgramRecord that doesn't have flags!  Use ProgramRecord.toProgram instead."

        WithFlags init ->
            Html.programWithFlags
                { init = init >> handleNever
                , update = record.update >>> handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }

        WithLocation init onLocationChange ->
            -- NOTE: same note as for NoArgs above
            Debug.crash "You are using ProgramRecord.toProgramWithFlags with a ProgramRecord that doesn't have flags!  Use ProgramRecord.toProgram instead."

        WithBoth init onLocationChange ->
            Navigation.programWithFlags
                onLocationChange
                { init = init >>> handleNever
                , update = record.update >>> handleNever
                , subscriptions = record.subscriptions
                , view = record.view
                }


(>>>) : (a -> b -> y) -> (y -> z) -> (a -> b -> z)
(>>>) y f a b =
    f (y a b)


{-| Creates a `ProgramRecord` with the configuration you would normally use with `elm-lang/html: Html.program`
-}
htmlProgram :
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }
    -> ProgramRecord Never Never model msg
htmlProgram config =
    { init = NoArgs (Err config.init)
    , update = config.update >>> Err
    , subscriptions = config.subscriptions
    , view = config.view
    }


{-| Creates a `ProgramRecord` with the configuration you would normally use with `elm-lang/html: Html.programWithFlags`
-}
htmlProgramWithFlags :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }
    -> ProgramRecord flags Never model msg
htmlProgramWithFlags config =
    { init = WithFlags (config.init >> Err)
    , update = config.update >>> Err
    , subscriptions = config.subscriptions
    , view = config.view
    }


{-| Creates a `ProgramRecord` with the configuration you would normally use with `elm-lang/navigation: Navigation.program`
-}
navigationProgram :
    (Location -> msg)
    ->
        { init : Location -> ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Html msg
        , subscriptions : model -> Sub msg
        }
    -> ProgramRecord Never Never model msg
navigationProgram onLocationChange config =
    { init = WithLocation (config.init >> Err) onLocationChange
    , update = config.update >>> Err
    , subscriptions = config.subscriptions
    , view = config.view
    }


{-| Creates a `ProgramRecord` with the configuration you would normally use with `elm-lang/navigation: Navigation.programWithFlags`
-}
navigationProgramWithFlags :
    (Location -> msg)
    ->
        { init : flags -> Location -> ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Html msg
        , subscriptions : model -> Sub msg
        }
    -> ProgramRecord flags Never model msg
navigationProgramWithFlags onLocationChange config =
    { init = WithBoth (config.init >>> Err) onLocationChange
    , update = config.update >>> Err
    , subscriptions = config.subscriptions
    , view = config.view
    }


{-| -}
completableProgram :
    { init : Result ( model, Cmd msg ) done
    , update : msg -> model -> Result ( model, Cmd msg ) done
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }
    -> ProgramRecord Never done model msg
completableProgram config =
    { init = NoArgs config.init
    , update = config.update
    , subscriptions = config.subscriptions
    , view = config.view
    }


handleNever : Result a Never -> a
handleNever r =
    case r of
        Ok a ->
            never a

        Err x ->
            x
