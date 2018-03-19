module Main exposing (..)

import AudioPlayer exposing (..)
import Color
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { states : List AudioPlayer.State
    }


init : ( Model, Cmd Msg )
init =
    ( { states = [ AudioPlayer.stateWithConfig config1, AudioPlayer.stateWithConfig config2 ] }, Cmd.none )


getIdFromConfig : Config msg -> String
getIdFromConfig config =
    case config of
        Config msg ->
            msg.id


getIdFromControlMsg : AudioPlayer.ControlMsg -> String
getIdFromControlMsg msg =
    case msg of
        Play id ->
            id

        Pause id ->
            id

        Seek id time ->
            id


type Msg
    = PlayerMsg AudioPlayer.State
    | ControlMsg AudioPlayer.ControlMsg
    | ErrorHandler String (Result AudioPlayer.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerMsg state ->
            let
                updateState : State -> State -> State
                updateState new old =
                    if old.id == new.id then
                        new
                    else
                        old
            in
            ( { model | states = List.map (updateState state) model.states }, Cmd.none )

        ControlMsg controlMsg ->
            let
                id =
                    getIdFromControlMsg controlMsg
            in
            ( model, AudioPlayer.defaultControls (ErrorHandler id) controlMsg )

        ErrorHandler id result ->
            let
                updateState : String -> Result Error () -> State -> State
                updateState id result old =
                    if old.id == id then
                        let
                            e =
                                case result of
                                    Err error ->
                                        Just error

                                    _ ->
                                        Nothing
                        in
                        { old | error = e }
                    else
                        old
            in
            ( { model | states = List.map (updateState id result) model.states }, Cmd.none )


config1 =
    AudioPlayer.config
        { id = "audioPlayer1"
        , updateMsg = PlayerMsg
        , controlMsg = ControlMsg
        , source = { media = [ MP3 "assets/Joplin.mp3" ], name = "Scott Joplin", title = "The Entertainer" }
        }


config2 =
    AudioPlayer.configWithCustomizations
        { id = "audioPlayer2"
        , updateMsg = PlayerMsg
        , controlMsg = ControlMsg
        , source = { media = [ MP3 "assets/Elm_Town_25.mp3" ], name = "Elm Town", title = "Episode 25" }
        , customizations = { colors = ( Color.darkOrange, Color.white ), loop = False, autoplay = False }
        }


view : Model -> Html Msg
view model =
    let
        stateFilter config =
            singleState (List.head <| List.filter (\state -> state.id == getIdFromConfig config) model.states) config

        singleState state config =
            case state of
                Just s ->
                    s

                Nothing ->
                    AudioPlayer.stateWithConfig config
    in
    div []
        [ div
            [ style [ ( "width", "600px" ), ( "margin", "100px" ) ] ]
            [ playerView config1 <| stateFilter config1
            ]
        , div [ style [ ( "width", "300px" ), ( "margin", "200px" ) ] ]
            [ playerView config2 <| stateFilter config2
            ]
        ]


playerView : AudioPlayer.Config msg -> State -> Html msg
playerView config state =
    AudioPlayer.audioPlayer config state


main =
    Html.program { update = update, view = view, init = init, subscriptions = always Sub.none }
