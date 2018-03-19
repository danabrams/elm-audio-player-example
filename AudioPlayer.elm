module AudioPlayer exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Html exposing (Html, audio)
import Html.Attributes exposing (autoplay, id, src, type_)
import Html.Events
import Json.Decode as Decode
import Media exposing (Error)
import Media.Events exposing (..)
import Media.State exposing (Playback(..), defaultAudio)
import Svg
import Svg.Attributes as SvgAttr
import Task


type alias State =
    { id : String
    , source : Source
    , playback : Playback
    , currentTime : Float
    , duration : Float
    , error : Maybe Media.Error
    }


stateWithConfig : Config msg -> State
stateWithConfig (Config { id, updateMsg, controlMsg, source, customizations }) =
    { id = id
    , source = source
    , playback = Paused
    , currentTime = 0
    , duration = 0
    , error = Nothing
    }


type alias Source =
    { media : List Media
    , name : String
    , title : String
    }


type Media
    = MP3 String
    | AAC String
    | Wav String
    | Ogg String
    | FLAC String
    | WebM String


type alias Error =
    Media.Error


type ControlMsg
    = Play String
    | Pause String
    | Seek String Float



-- CONFIG


{-| Configuration for your audio player.
**Note:** Your `Config` should _never_ be held in your model.
It should only appear in `view` code.
-}
type Config msg
    = Config
        { id : String
        , updateMsg : State -> msg
        , controlMsg : ControlMsg -> msg
        , source : Source
        , customizations : Customizations
        }


config :
    { id : String
    , updateMsg : State -> msg
    , controlMsg : ControlMsg -> msg
    , source : Source
    }
    -> Config msg
config { id, updateMsg, controlMsg, source } =
    Config
        { id = id
        , updateMsg = updateMsg
        , controlMsg = controlMsg
        , source = source
        , customizations = defaultCustomizations
        }


configWithCustomizations :
    { id : String
    , updateMsg : State -> msg
    , controlMsg : ControlMsg -> msg
    , source : Source
    , customizations : Customizations
    }
    -> Config msg
configWithCustomizations { id, updateMsg, controlMsg, source, customizations } =
    Config
        { id = id
        , updateMsg = updateMsg
        , controlMsg = controlMsg
        , source = source
        , customizations = customizations
        }


type alias Customizations =
    { colors : ( Color.Color, Color.Color )
    , loop : Bool
    , autoplay : Bool
    }


defaultCustomizations : Customizations
defaultCustomizations =
    { colors = ( charcoal, white )
    , loop = False
    , autoplay = False
    }



-- VIEW


errorToString : Media.Error -> String
errorToString error =
    case error of
        Media.State.NotFound id ->
            "Not Found: " ++ id

        Media.State.NotMediaElement id class ->
            "Not a Media Element: " ++ ("Element '#" ++ id ++ "' is an instance of '" ++ class ++ "")

        Media.State.PlayPromiseFailure error ->
            "Media.play() promise failed with the following message: " ++ error

        Media.State.NotTimeRanges class ->
            "Value passed to runtime decoder of TimeRanges was not a TimeRanges object, but rather of " ++ class

        Media.State.DecodeError error ->
            "Error decoding the Media State: " ++ error


sourceToSrc : Media -> Html msg
sourceToSrc media =
    case media of
        MP3 url ->
            Html.source [ src url, type_ "audio/mpeg" ] [ Html.text "Your browser does not support playback of the MP3 format. Please try another browser, such as Chrome." ]

        Ogg url ->
            Html.source [ src url, type_ "audio/ogg" ] [ Html.text "Your browser does not support playback of the Ogg format. Please try another browser, such as Chrome." ]

        Wav url ->
            Html.source [ src url, type_ "audio/wav" ] [ Html.text "Your browser does not support playback of the MP3 format. Please try another browser, such as Chrome." ]

        WebM url ->
            Html.source [ src url, type_ "audio/webm" ] [ Html.text "Your browser does not support playback of the MP3 format. Please try another browser, such as Chrome." ]

        AAC url ->
            Html.source [ src url, type_ "audio/mp4" ] [ Html.text "Your browser does not support playback of the MP3 format. Please try another browser, such as Safari." ]

        FLAC url ->
            Html.source [ src url, type_ "audio/flac" ] [ Html.text "Your browser does not support playback of the MP3 format. Please try another browser, such as Chrome." ]


source : List Media -> Source
source media =
    { media = media, name = "", title = "" }


colorToString : Color.Color -> String
colorToString color =
    let
        c =
            toRgb color
    in
    "rgb(" ++ toString c.red ++ "," ++ toString c.green ++ "," ++ toString c.blue ++ ")"


audioPlayer : Config msg -> State -> Html msg
audioPlayer (Config { id, updateMsg, controlMsg, source, customizations }) state =
    let
        playPauseIcon : String -> Element msg
        playPauseIcon =
            case state.playback of
                Playing ->
                    pauseIcon

                _ ->
                    playIcon

        currentTime =
            state.currentTime

        duration =
            state.duration

        remainingTime =
            duration - currentTime

        ( color1, color2 ) =
            customizations.colors

        ( played, unplayed ) =
            case duration > 0 of
                True ->
                    ( width <| fillPortion <| floor (currentTime * 100), width <| fillPortion <| floor (remainingTime * 100) )

                False ->
                    ( width <| fillPortion 0, width <| fillPortion 100 )

        playPauseTask =
            case state.playback of
                Playing ->
                    controlMsg <| Pause state.id

                _ ->
                    controlMsg <| Play state.id

        seekTask time =
            controlMsg <| Seek state.id time
    in
    Element.layout [] <|
        column [ width fill, height (px 50), Border.color color1, Border.shadow { blur = 3, color = color1, offset = ( 0, 0 ), size = 0.5 }, Border.rounded 5, clipX ]
            [ el [] <|
                html <|
                    audio
                        [ Html.Attributes.id id
                        , if customizations.loop then
                            Html.Attributes.loop True
                          else
                            Html.Attributes.loop False
                        , if customizations.autoplay then
                            Html.Attributes.autoplay True
                          else
                            Html.Attributes.autoplay False
                        , onDurationChange <| mediaEvent updateMsg state
                        , onTimeUpdate <| mediaEvent updateMsg state
                        , onSeeked <| mediaEvent updateMsg state
                        , onPaused <| mediaEvent updateMsg state
                        , onPlaying <| mediaEvent updateMsg state
                        , onStalled <| mediaEvent updateMsg state
                        , onLoadedData <| mediaEvent updateMsg state
                        , Media.playbackRate 1
                        ]
                        (List.map sourceToSrc source.media)
            , row
                [ width fill, height fill ]
                [ el [ width (px 50), height fill, Background.color color1, padding 15, onClick playPauseTask ] <| playPauseIcon <| colorToString color2
                , row
                    [ width fill
                    , height fill
                    , clipX
                    , inFront <|
                        el
                            [ width fill
                            , height fill
                            , onMouseDown (\x -> seekTask <| x * duration)
                            ]
                            empty
                    , behind <|
                        row [ width fill, height fill, Background.color color2, Font.color color1, clipX ]
                            [ el [ width (px 50), height (px 12), alignLeft, Font.alignLeft, paddingXY 10 0, Font.size 10 ] <|
                                text <|
                                    Media.timeToString currentTime
                            , column [ width (px 100), height (px 24), centerY, Font.alignLeft, Font.size 12 ]
                                [ el [ Font.bold, alignLeft ] <| text source.name
                                , el [ alignLeft ] <| text source.title
                                ]
                            ]
                    ]
                    [ row
                        [ height fill
                        , played
                        , Background.color color1
                        , clipX
                        , Font.color color2
                        ]
                        [ el [ width (px 50), height (px 12), alignLeft, Font.alignLeft, paddingXY 10 0, Font.color color2, Font.size 10 ] <|
                            text <|
                                Media.timeToString currentTime
                        , column [ width (px 100), height (px 24), centerY, Font.alignLeft, Font.size 12 ]
                            [ el [ Font.bold, alignLeft, centerY ] <| text source.name
                            , el [ alignLeft, centerY ] <| text source.title
                            ]
                        ]
                    , el [ unplayed, height fill, transparent True, clipX ]
                        empty
                    ]
                , column [ width (px 50), height (px 50), Background.color color1 ]
                    [ el [ width fill, height (px 12), Font.center, clipX, centerY, Font.size 10, Font.color color2 ] <| text <| Media.timeToString remainingTime ]
                ]
            ]


playIcon : String -> Element msg
playIcon color =
    html <|
        Svg.svg [ SvgAttr.viewBox "0 0 36 36" ]
            [ Svg.polygon
                [ SvgAttr.fill color
                , SvgAttr.points "4 0 4 36 36 18"
                ]
                []
            ]


pauseIcon : String -> Element msg
pauseIcon color =
    html <|
        Svg.svg [ SvgAttr.viewBox "0 0 36 36" ]
            [ Svg.g [ SvgAttr.fill color ]
                [ Svg.rect [ SvgAttr.x "4", SvgAttr.y "0", SvgAttr.rx "3", SvgAttr.width "12", SvgAttr.height "36" ] []
                , Svg.rect [ SvgAttr.x "22", SvgAttr.y "0", SvgAttr.rx "3", SvgAttr.width "12", SvgAttr.height "36" ] []
                ]
            ]



--CUSTOM EVENTS


onMouseDown : (Float -> msg) -> Attribute msg
onMouseDown tagger =
    htmlAttribute <|
        Html.Events.on "mousedown" <|
            Decode.map tagger <|
                Decode.map3
                    (\x y z -> toFloat (x - y) / toFloat z)
                    (Decode.at [ "offsetX" ] Decode.int)
                    (Decode.at [ "target", "offsetLeft" ] Decode.int)
                    (Decode.at [ "target", "offsetWidth" ] Decode.int)


mediaEvent : (State -> msg) -> State -> (Media.State.State -> msg)
mediaEvent updateMsg state =
    let
        newState : Media.State.State -> msg
        newState newMedia =
            updateMsg
                { state
                    | currentTime = newMedia.currentTime
                    , duration = newMedia.duration
                    , playback = newMedia.playback
                }
    in
    newState


defaultErrorHandler : Result Media.Error () -> Maybe Media.Error
defaultErrorHandler result =
    case result of
        Err error ->
            let
                handleError =
                    Debug.log "Media Task Error: " <| errorToString error
            in
            Just error

        _ ->
            Nothing


setError : State -> Result Error () -> State
setError state error =
    { state | error = defaultErrorHandler error }


defaultControls : (Result Error () -> msg) -> ControlMsg -> Cmd msg
defaultControls errorHandler controlMsg =
    case controlMsg of
        Play id ->
            Task.attempt errorHandler <| Media.play id

        Pause id ->
            Task.attempt errorHandler <| Media.pause id

        Seek id time ->
            Task.attempt errorHandler <| Media.seek id time
