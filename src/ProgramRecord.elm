module ProgramRecord
    exposing
        ( AndThenModel
        , AndThenMsg
        , CacheModel
        , CacheMsg
        , ProgramRecord
        , andThen
        , applyFlags
        , cache
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


## Transforming programs

@docs applyFlags, cache, CacheModel, CacheMsg


## Combining programs

@docs andThen, AndThenModel, AndThenMsg

-}

import Html exposing (Html)
import Navigation exposing (Location)
import Process
import Task exposing (Task)


{-| `update` and `init` return a `Result` where:

  - `Ok` means auth is finished; the provided value will be used to initialize the main program
  - `Err` means auth did not finish; the provided value is the new model and Cmd for the auth program

-}
type alias ProgramRecord flags done model msg =
    -- TODO: make this private
    { init : ProgramType flags (Result ( model, Cmd msg ) done) msg
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


mapProgramType : (a -> b) -> ProgramType flags a msg -> ProgramType flags b msg
mapProgramType f t =
    case t of
        NoArgs init ->
            NoArgs (f init)

        WithFlags init ->
            WithFlags (f << init)

        WithLocation init onLocation ->
            WithLocation (f << init) onLocation

        WithBoth init onLocation ->
            WithBoth (init >>> f) onLocation


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


{-| Provide flags to a ProgramRecord, producing a ProgramRecord that doesn't need flags
-}
applyFlags : flags -> ProgramRecord flags done model msg -> ProgramRecord Never done model msg
applyFlags flags record =
    let
        newInit =
            case record.init of
                NoArgs _ ->
                    -- We could only get here if the caller provided a value of type `Never`!
                    Debug.crash "applyFlags: You tried to give flags to a ProgramRecord that doesn't accept flags!"

                WithLocation _ _ ->
                    -- We could only get here if the caller provided a value of type `Never`!
                    Debug.crash "applyFlags: You tried to give flags to a ProgramRecord that doesn't accept flags!"

                WithFlags init ->
                    NoArgs (init flags)

                WithBoth init onLocation ->
                    WithLocation (init flags) onLocation
    in
    { init = newInit
    , update = record.update
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


{-| -}
type AndThenModel model1 model2
    = First Location model1
    | Second model2


{-| -}
type AndThenMsg msg1 msg2
    = AndThenLocationChange Location
    | FirstMsg msg1
    | SecondMsg msg2
    | IgnoreMsg


{-|

  - TODO: allow `second` to have `done /= Never`
  - TODO: allow `first` to have `flags /= Never`

-}
andThen : ProgramRecord a Never model2 msg2 -> ProgramRecord Never a model1 msg1 -> ProgramRecord Never Never (AndThenModel model1 model2) (AndThenMsg msg1 msg2)
andThen second first =
    let
        init :
            ProgramRecord flags a model1 msg1
            -> ProgramRecord a Never model2 msg2
            -> Maybe flags
            -> Location
            -> ( AndThenModel model1 model2, Cmd (AndThenMsg msg1 msg2) )
        init first second flags location =
            applyInit first.init flags location
                |> handleFirstResult location

        handleFirstResult :
            Location
            -> Result ( model1, Cmd msg1 ) a
            -> ( AndThenModel model1 model2, Cmd (AndThenMsg msg1 msg2) )
        handleFirstResult location result =
            case result of
                Err ( authModel, authCmd ) ->
                    ( First location authModel
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
            -> AndThenModel model1 model2
            -> ( AndThenModel model1 model2, Cmd (AndThenMsg msg1 msg2) )
        update msg model =
            case model of
                First location authModel ->
                    case msg of
                        AndThenLocationChange newLocation ->
                            case getLocationChange first.init of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just onLocationChange ->
                                    first.update (onLocationChange newLocation) authModel
                                        |> handleFirstResult newLocation

                        FirstMsg authMsg ->
                            first.update authMsg authModel
                                |> handleFirstResult location

                        SecondMsg mainMsg ->
                            Debug.crash "ProgramWithAuth.update: got a MainMsg before auth finished. (This should not be possible.)" ( msg, model )

                        IgnoreMsg ->
                            ( model, Cmd.none )

                Second mainModel ->
                    case msg of
                        AndThenLocationChange location ->
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
            AndThenModel model1 model2
            -> Sub (AndThenMsg msg1 msg2)
        subscriptions model =
            case model of
                First _ authModel ->
                    first.subscriptions authModel
                        |> Sub.map FirstMsg

                Second mainModel ->
                    second.subscriptions mainModel
                        |> Sub.map SecondMsg

        view :
            AndThenModel model1 model2
            -> Html (AndThenMsg msg1 msg2)
        view model =
            case model of
                First _ authModel ->
                    first.view authModel
                        |> Html.map FirstMsg

                Second mainModel ->
                    second.view mainModel
                        |> Html.map SecondMsg
    in
    navigationProgram
        AndThenLocationChange
        { init = init first second Nothing
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--
-- Cache
--


{-| -}
type CacheModel model
    = Loading Location
    | Loaded model
    | Writing


{-| -}
type CacheMsg done msg
    = ReadResult (Result () done)
    | LoadedMsg msg
    | CacheLocationChange Location
    | WriteSpawned done


{-| Caches the completion value of the given `ProgramRecord` using the `read` and `write` Tasks that you provide.

When the resulting program record starts, it will use the `read` task.
If the read succeeds, the program will immediately complete with the read value.
If the read fails, the provided program will run to completion, after which the result will be saved with the provided `write` task.

If your `read` tasks takes a long time, you should provide a `loadingView` to be displayed while waiting for the task to finish.

  - TODO: make this work with `flags /= Never`

-}
cache :
    { read : Task () done
    , write : done -> Task Never ()
    , loadingView : Maybe (Html Never)
    }
    -> ProgramRecord Never done model msg
    -> ProgramRecord Never done (CacheModel model) (CacheMsg done msg)
cache config program =
    let
        update msg model =
            case model of
                Loading location ->
                    case msg of
                        LoadedMsg _ ->
                            Debug.crash "Internal error: ProgramRecord.cache got a LoadedMsg while model == Loading"

                        WriteSpawned done ->
                            Debug.crash "Internal error: ProgramRecord.cache got a WriteSpawned while model == Loading"

                        CacheLocationChange location ->
                            Err ( Loading location, Cmd.none )

                        ReadResult (Err ()) ->
                            applyInit program.init Nothing location
                                |> Result.mapError (Tuple.mapFirst Loaded >> Tuple.mapSecond (Cmd.map LoadedMsg))

                        ReadResult (Ok done) ->
                            Ok done

                Loaded model ->
                    case msg of
                        ReadResult _ ->
                            Debug.crash "Internal error: ProgramRecord.cache: read task resolved more than once!"

                        WriteSpawned _ ->
                            Debug.crash "Internal error: ProgramRecord.cache: got a WriteSpawned while model == Loaded"

                        CacheLocationChange location ->
                            getLocationChange program.init
                                |> Maybe.map (\f -> f location)
                                |> Maybe.map (\m -> update (LoadedMsg m) (Loaded model))
                                |> Maybe.withDefault (Err ( Loaded model, Cmd.none ))

                        LoadedMsg msg ->
                            case program.update msg model of
                                Err next ->
                                    next
                                        |> Tuple.mapFirst Loaded
                                        |> Tuple.mapSecond (Cmd.map LoadedMsg)
                                        |> Err

                                Ok done ->
                                    Err
                                        ( Writing
                                        , config.write done
                                            |> Process.spawn
                                            |> Task.perform (\_ -> WriteSpawned done)
                                        )

                Writing ->
                    case msg of
                        ReadResult _ ->
                            Debug.crash "Internal error: ProgramRecord.cache: read task resolved more than once!"

                        CacheLocationChange location ->
                            -- We don't need location after writing, so we can safely ignore
                            Err ( Writing, Cmd.none )

                        LoadedMsg _ ->
                            Debug.crash "Internal error: ProgramRecord.cache got a LoadedMsg while model == Loading"

                        WriteSpawned done ->
                            Ok done
    in
    { init =
        flip WithLocation CacheLocationChange <|
            \location ->
                Err
                    ( Loading location
                    , config.read |> Task.attempt ReadResult
                    )
    , update = update
    , subscriptions =
        \model ->
            case model of
                Loading _ ->
                    Sub.none

                Writing ->
                    Sub.none

                Loaded model ->
                    program.subscriptions model
                        |> Sub.map LoadedMsg
    , view =
        \model ->
            case model of
                Loading _ ->
                    config.loadingView
                        |> Maybe.withDefault (Html.text "")
                        |> Html.map never

                Writing ->
                    Html.text ""

                Loaded model ->
                    program.view model
                        |> Html.map LoadedMsg
    }


handleNever : Result a Never -> a
handleNever r =
    case r of
        Ok a ->
            never a

        Err x ->
            x
