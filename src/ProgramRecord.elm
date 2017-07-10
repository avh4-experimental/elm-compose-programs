module ProgramRecord
    exposing
        ( ProgramRecord
        , andThen
        , completableProgram
        , htmlProgram
        , htmlProgramWithFlags
        , navigationProgram
        , navigationProgramWithFlags
        , toProgram
        , toProgramWithFlags
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


## Combining programs

@docs andThen

-}

import Html exposing (Html)
import Navigation exposing (Location)
import Task exposing (Task)


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



--
-- AndThen
--


type AndThenModel flags model1 model2
    = First (Maybe flags) Location model1
    | Second model2


type AndThenMsg msg1 msg2
    = LocationChange Location
    | FirstMsg msg1
    | SecondMsg msg2
    | IgnoreMsg


{-|

  - TODO: allow `second` to have `done /= Never`
  - TODO: allow `first` to have `flags /= Never`

-}
andThen : ProgramRecord a Never model2 msg2 -> ProgramRecord Never a model1 msg1 -> ProgramRecord Never Never (AndThenModel Never model1 model2) (AndThenMsg msg1 msg2)
andThen second first =
    let
        init :
            ProgramRecord flags a model1 msg1
            -> ProgramRecord a Never model2 msg2
            -> Maybe flags
            -> Location
            -> ( AndThenModel flags model1 model2, Cmd (AndThenMsg msg1 msg2) )
        init first second flags location =
            applyInit first.init flags location
                |> handleFirstResult flags location

        handleFirstResult :
            Maybe flags
            -> Location
            -> Result ( model1, Cmd msg1 ) a
            -> ( AndThenModel flags model1 model2, Cmd (AndThenMsg msg1 msg2) )
        handleFirstResult flags location result =
            case result of
                Err ( authModel, authCmd ) ->
                    ( First flags location authModel
                    , authCmd
                        |> Cmd.map FirstMsg
                    )

                Ok firstDone ->
                    let
                        ( mainModel, cmd ) =
                            applyInit second.init (Just firstDone) location
                                |> handleNever
                    in
                    ( Second mainModel
                    , Cmd.map SecondMsg cmd
                    )

        update :
            AndThenMsg msg1 msg2
            -> AndThenModel flags model1 model2
            -> ( AndThenModel flags model1 model2, Cmd (AndThenMsg msg1 msg2) )
        update msg model =
            case model of
                First flags location authModel ->
                    case msg of
                        LocationChange newLocation ->
                            case getLocationChange first.init of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just onLocationChange ->
                                    first.update (onLocationChange newLocation) authModel
                                        |> handleFirstResult flags newLocation

                        FirstMsg authMsg ->
                            first.update authMsg authModel
                                |> handleFirstResult flags location

                        SecondMsg mainMsg ->
                            Debug.crash "ProgramWithAuth.update: got a MainMsg before auth finished. (This should not be possible.)" ( msg, model )

                        IgnoreMsg ->
                            ( model, Cmd.none )

                Second mainModel ->
                    case msg of
                        LocationChange location ->
                            getLocationChange second.init
                                |> Maybe.map (\f -> f location)
                                |> Maybe.map (\m -> update (SecondMsg m) model)
                                |> Maybe.withDefault ( model, Cmd.none )

                        FirstMsg authMsg ->
                            Debug.log "ProgramWithAuth.update: got an AuthMsg after auth finished (this could happen if the auth program started a Task or Cmd that did not complete until after the auth finished in some other way)" ( msg, model )
                                |> always ( model, Cmd.none )

                        SecondMsg mainMsg ->
                            second.update mainMsg mainModel
                                |> handleNever
                                |> Tuple.mapFirst Second
                                |> Tuple.mapSecond (Cmd.map SecondMsg)

                        IgnoreMsg ->
                            ( model, Cmd.none )

        subscriptions :
            AndThenModel flags model1 model2
            -> Sub (AndThenMsg msg1 msg2)
        subscriptions model =
            case model of
                First _ _ authModel ->
                    first.subscriptions authModel
                        |> Sub.map FirstMsg

                Second mainModel ->
                    second.subscriptions mainModel
                        |> Sub.map SecondMsg

        view :
            AndThenModel flags model1 model2
            -> Html (AndThenMsg msg1 msg2)
        view model =
            case model of
                First _ _ authModel ->
                    first.view authModel
                        |> Html.map FirstMsg

                Second mainModel ->
                    second.view mainModel
                        |> Html.map SecondMsg
    in
    navigationProgram
        LocationChange
        { init = init first second Nothing
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


handleNever : Result a Never -> a
handleNever r =
    case r of
        Ok a ->
            never a

        Err x ->
            x
