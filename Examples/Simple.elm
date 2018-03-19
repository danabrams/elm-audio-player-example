module Main exposing (..)

import AudioPlayer exposing (..)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Task


type alias Model =
    State


init =
    ( stateWithConfig config, Cmd.none )


type Msg
    = PlayerMsg AudioPlayer.State
    | ControlMsg AudioPlayer.ControlMsg
    | ErrorHandler (Result Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerMsg state ->
            ( state, Cmd.none )

        ControlMsg controlMsg ->
            ( model, AudioPlayer.defaultControls ErrorHandler controlMsg )

        ErrorHandler result ->
            ( { model | error = AudioPlayer.defaultErrorHandler result }, Cmd.none )


config =
    AudioPlayer.config
        { id = "audioPlayer"
        , updateMsg = PlayerMsg
        , controlMsg = ControlMsg
        , source =
            { media = [ MP3 "assets/Joplin.mp3" ]
            , name = "Scott Joplin"
            , title = "The Entertainer"
            }
        }


view : Model -> Html Msg
view model =
    div []
        [ div
            [ style [ ( "width", "600px" ), ( "margin", "auto" ), ( "marginTop", "100px" ) ] ]
            [ AudioPlayer.audioPlayer config model ]
        ]


main =
    Html.program { update = update, view = view, init = init, subscriptions = always Sub.none }
