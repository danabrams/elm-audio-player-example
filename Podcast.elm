module Podcast exposing (..)

--import Time.DateTime exposing (DateTime, fromTimestamp, hour, minute, second)

import Color exposing (black, white)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Attribute, Html, div)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode
import Media exposing (Error, muted, play)
import Media.Events exposing (onDurationChange, onLoadedData, onPaused, onPlaying, onProgress, onTimeUpdate)
import Media.State as Media
import Svg exposing (svg)
import Svg.Attributes as SvgAttr
import Svg.Events
import Task
import Window


type alias Model =
    { episode : Episode
    , podcast : Podcast
    , duration : Float
    , currentTime : Float
    , device : Device
    , state : Media.State
    , settings : OpenClosed
    , playbackRate : Float
    }


type Device
    = Unknown
    | Phone { orientation : Orientation, size : Window.Size }
    | Tablet { orientation : Orientation, size : Window.Size }
    | Desktop { orientation : Orientation, size : Window.Size }


type Orientation
    = Portrait
    | Landscape


type alias Enclosure =
    { url : String
    , length : String
    , mediaType : String
    }


type alias Podcast =
    { title : String
    , home : String
    }


type alias Episode =
    { title : String
    , link : String
    , description : String
    , enclosure : Enclosure
    , episodeNumber : Int
    , poster : String
    , showNotes : String
    }


type Msg
    = NoOp
    | Resize Window.Size
    | MediaUpdate Media.State
    | Play
    | HandleError (Result Error ())
    | Seek Float
    | Pause
    | ToggleSettings
    | ChangePlaybackRate Float


type OpenClosed
    = Open
    | Closed


init : ( Model, Cmd Msg )
init =
    ( { episode =
            { title = "Elm Town 27 - Murphy Randle's Story"
            , link = "http://elmtown.audio/27-murphy-randle"
            , description = "Surprise! Mario Rogic is your host for this episode, because he's interviewing the normal host of the podcast, Murphy Randle."
            , enclosure =
                { url = "https://audio.simplecast.com/27550e4a.mp3"

                --url = "/assets/Elm_Town_25.mp3"
                , length = "40985895"
                , mediaType = "audio/mpeg"
                }
            , episodeNumber = 27
            , poster = "https://media.simplecast.com/episode/image/111821/1518046334-artwork.jpg"
            , showNotes = "<p>Surprise! Mario Rogic is your host for this episode, because he's interviewing the normal host of the podcast, Murphy Randle.  Listen to hear about Murphy's background in Animation, and how Murphy came to the world of Web development, and eventually started Elm Town!</p>\n\n<a name='Links'></a>\n<h1>Links</h1>\n\n<ul>\n<li>(00:25:50) <a href='https://www.youtube.com/watch?v=-JlC2Q89yg4'>Climbing Into Elm </a></li>\n</ul>\n\n\n<a name='Picks'></a>\n<h2>Picks</h2>\n\n<ul>\n<li>(00:39:15) <a href='https://www.scalyr.com'>Scalyr</a></li>\n<li>(00:40:19) <a href='https://www.patreon.com/towncasts'>Our Patreon</a></li>\n<li>(00:40:37) <a href='https://reason.town/'>Reason Town</a></li>\n</ul>"
            }
      , podcast = { title = "Elm Town", home = "Https://Elmtown.audio" }
      , device = Unknown
      , duration = 0
      , currentTime = 0
      , state = Media.defaultAudio "podcastPlayer"
      , settings = Closed
      , playbackRate = 1.0
      }
    , Task.perform Resize Window.size
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            let
                device =
                    processWindowSize size
            in
            ( { model | device = device }, Cmd.none )

        MediaUpdate state ->
            ( { model
                | duration = state.duration
                , currentTime = state.currentTime
                , state = state
              }
            , Cmd.none
            )

        Play ->
            ( model, Task.attempt HandleError <| Media.play "podcastPlayer" )

        Pause ->
            ( model, Task.attempt HandleError <| Media.pause "podcastPlayer" )

        HandleError error ->
            ( model, Cmd.none )

        Seek time ->
            ( model, Task.attempt HandleError <| Media.seek "podcastPlayer" time )

        ToggleSettings ->
            case model.settings of
                Open ->
                    ( { model | settings = Closed }, Cmd.none )

                Closed ->
                    ( { model | settings = Open }, Cmd.none )

        ChangePlaybackRate rate ->
            ( { model | playbackRate = rate }, Cmd.none )

        _ ->
            ( model, Cmd.none )


processWindowSize : Window.Size -> Device
processWindowSize size =
    let
        w =
            size.width

        h =
            size.height

        p =
            w < h
    in
    case p of
        True ->
            if w <= 1000 then
                Phone { orientation = Portrait, size = size }
            else if w <= 1200 then
                Tablet { orientation = Portrait, size = size }
            else
                Desktop { orientation = Portrait, size = size }

        False ->
            if h <= 1000 then
                Phone { orientation = Landscape, size = size }
            else if h <= 1200 then
                Tablet { orientation = Landscape, size = size }
            else
                Desktop { orientation = Landscape, size = size }



-- Views


view : Model -> Html Msg
view model =
    case model.device of
        Unknown ->
            div [] []

        Phone _ ->
            phoneView model

        _ ->
            phoneView model


phoneView : Model -> Html Msg
phoneView model =
    Element.layout
        [ Background.color white
        ]
    <|
        case model.device of
            _ ->
                column
                    [ width fill
                    , height fill
                    , alignTop
                    , inFront <| settingsView model
                    ]
                    [ stickyPlayerView model
                    , episodeView model
                    , logoView model
                    , audioPlayer model
                    ]


logoView : Model -> Element msg
logoView model =
    row [ width fill, height (px 80), alignBottom, paddingXY 20 0, Background.color <| Color.darkBlue, Font.color white ]
        [ el [ Font.alignLeft, alignLeft, Font.bold, Font.size 54 ] (text "Elmcast")
        , el [ Font.alignRight, alignRight, Font.size 54, centerY, paddingEach { right = 20, left = 0, bottom = 0, top = 0 } ] (text "+")
        ]


episodeView : Model -> Element msg
episodeView model =
    let
        viewHeight =
            case model.device of
                Phone device ->
                    device.size.height - (201 + 94)

                _ ->
                    800
    in
    paragraph [ width fill, height (px viewHeight), scrollbarY, spacing 5, Font.alignLeft, Font.size 30, paddingXY 50 0, spacing 5, scrollbarY, alignTop ] [ html <| textHtml model.episode.showNotes ]


stickyPlayerView : Model -> Element Msg
stickyPlayerView model =
    let
        posterSize =
            210

        controls =
            row [ width fill, height (px posterSize), spacingXY 10 0 ]
                [ posterImage model posterSize
                , column
                    [ width fill
                    , height (px posterSize)
                    , centerX
                    , paddingXY 0 5
                    , spacingXY 0 25
                    ]
                    [ smallTitle model
                    , controlRowView model
                    , el [ width fill, alignBottom ] <| playerTimes model
                    ]
                ]
    in
    column
        [ width fill
        , height (px <| posterSize + 1)
        , Border.shadow { offset = ( 0, 6 ), blur = 10, color = Color.charcoal, size = 6 }
        ]
        [ controls
        , playHead model
        ]


settingsView : Model -> Element Msg
settingsView model =
    row
        [ width fill
        , height (px 150)
        , centerY
        , centerX
        , if model.settings == Closed then
            transparent True
          else
            transparent False
        ]
        [ el [ width (px 50) ] empty
        , column
            [ width (px 600)
            , height fill
            , centerX
            , Background.color Color.white
            , Border.shadow { offset = ( 2, 2 ), blur = 10, color = Color.charcoal, size = 3 }
            ]
            [ el [ centerX, padding 25, inFront <| el [ width fill, alignRight, alignTop, Font.size 44, Font.alignRight, Font.color Color.darkGray, onClick ToggleSettings ] <| text "x" ] <|
                Input.radioRow [ width fill, spacing 20, padding 10, Font.center, centerY, Font.size 32 ]
                    { onChange = Just ChangePlaybackRate
                    , selected = Just model.playbackRate
                    , label = Input.labelAbove [ Font.center, centerX, Font.bold, Font.size 32 ] (text "Playback Rate")
                    , options =
                        [ Input.option 0.5 <| text "0.5x"
                        , Input.option 1.0 (text "1x")
                        , Input.option 1.25 (text "1.25x")
                        , Input.option 1.5 (text "1.5x")
                        , Input.option 2.0 (text "2.0x")
                        ]
                    }

            {- } el [ width fill, centerX, Font.size 32, Font.bold, Font.center ] <|
                   text "Playback Rate"
               , el
                   [ width fill, padding 50 ]
                 <|
                   html <|
                       Html.input
                           [ Html.Attributes.type_ "range"
                           , Html.Attributes.min "0.5"
                           , Html.Attributes.max "2.0"
                           , Html.Attributes.value <| toString model.playbackRate
                           , Html.Attributes.step "0.25"
                           ]
                           []
            -}
            ]
        , el [ width (px 50) ] empty
        ]


playerTimes : Model -> Element msg
playerTimes model =
    let
        titleSize =
            24
    in
    row [ width fill, height (px titleSize), centerX ]
        [ el
            [ alignLeft
            , Font.alignLeft
            , Font.color Color.darkGray
            , Font.size titleSize
            , Font.italic
            ]
            (text <| timeToString model.currentTime)
        , el
            [ alignRight
            , Font.alignRight
            , Font.color Color.darkGray
            , Font.italic
            , Font.size titleSize
            , padding 10
            ]
            (text <| "-" ++ timeToString (model.duration - model.currentTime))
        ]


audioPlayer : Model -> Element Msg
audioPlayer model =
    html <|
        Html.audio
            [ Html.Attributes.id "podcastPlayer"
            , onPlaying MediaUpdate
            , onTimeUpdate MediaUpdate
            , onPaused MediaUpdate
            , onDurationChange MediaUpdate
            , onLoadedData MediaUpdate
            , onProgress MediaUpdate
            , Media.playbackRate model.playbackRate
            ]
            [ Html.source
                [ Html.Attributes.src model.episode.enclosure.url
                , Html.Attributes.type_ model.episode.enclosure.mediaType
                ]
                []
            ]


smallTitle : Model -> Element msg
smallTitle model =
    let
        titleSize =
            24
    in
    column [ width fill, paddingXY 10 0, height (px (titleSize * 2)), alignTop, alignLeft, Font.alignLeft, Font.size titleSize ]
        [ el [ width fill, Font.bold ] (text model.podcast.title)
        , el [ width fill, Font.italic ] (text model.episode.title)
        ]


largeTitle : Model -> Element msg
largeTitle model =
    paragraph [ width fill, height fill ]
        [ link [ width fill, Font.size 48, Font.center, Font.bold ]
            { url = model.podcast.home, label = text model.podcast.title }
        , link [ Font.size 36, Font.alignLeft, Font.italic, Font.color Color.darkBlue ]
            { url = model.episode.link, label = text model.episode.title }
        ]


isPhablet : Device -> Bool
isPhablet device =
    case device of
        Phone d ->
            case d.orientation of
                Portrait ->
                    d.size.width > 750

                Landscape ->
                    d.size.height > 750

        _ ->
            False


controlRowView : Model -> Element Msg
controlRowView model =
    let
        size =
            72

        iconSize =
            px size

        fontSize =
            size // 3

        playPauseButton =
            case model.state.playback of
                Media.Playing ->
                    el [ height iconSize, centerY, width iconSize, inFront <| el [ width fill, height fill, onClick <| Pause ] empty ] (html <| pauseIcon "black")

                _ ->
                    el [ height iconSize, centerY, width iconSize, inFront <| el [ width fill, height fill, onClick <| Play ] empty ] (html <| playIcon "black")
    in
    row
        [ width fill, height iconSize, spaceEvenly, centerY, paddingXY (size // 3) 0 ]
        [ el [ height iconSize, centerY, width iconSize, inFront <| el [ width fill, height fill, onClick <| Seek (model.currentTime - 30) ] empty ] (html <| backwardIcon "black")
        , playPauseButton
        , el [ height iconSize, centerY, width iconSize, inFront <| el [ width fill, height fill, onClick <| Seek (model.currentTime + 30) ] empty ] (html <| forwardIcon "black")
        , el [ Font.alignRight, centerY, width iconSize, height (px fontSize), Font.size fontSize, onClick ToggleSettings ] (text "Settings")
        ]


posterImage : Model -> Int -> Element msg
posterImage model size =
    image
        [ width (px size)
        , height (px size)
        ]
        { src = model.episode.poster
        , description = "Poster Image for episode " ++ toString model.episode.episodeNumber
        }


textHtml : String -> Html msg
textHtml html =
    div
        [ Json.Encode.string html
            |> Html.Attributes.property "innerHTML"
        ]
        []


playHead : Model -> Element Msg
playHead model =
    row
        [ width fill
        , height (px 10)
        , Background.color Color.lightGray
        , inFront <|
            row
                [ width fill
                , height fill
                , inFront <|
                    row [ width fill, height fill ]
                        [ played model ]
                ]
                [ buffered model ]
        ]
        []


buffered : Model -> Element Msg
buffered model =
    let
        range =
            case List.head model.state.timeRanges.buffered of
                Nothing ->
                    { start = 0, end = 0 }

                Just r ->
                    r

        unbuffered1 =
            range.start - 0

        buffered1 =
            range.end - range.start

        unbuffered2 =
            model.state.duration - range.end
    in
    row [ width fill, height fill, inFront <| el [ width fill, height fill, onMouseDown (\x -> Seek <| x * model.state.duration) ] empty ]
        [ el
            [ width (fillPortion <| floor unbuffered1), height fill, transparent True ]
            empty
        , el [ width (fillPortion <| floor buffered1), height fill, Background.color Color.darkGray ] empty
        , el [ width (fillPortion <| floor unbuffered2), height fill, transparent True ] empty
        ]


onMouseDown : (Float -> msg) -> Element.Attribute msg
onMouseDown tagger =
    htmlAttribute <|
        Html.Events.on "mousedown" <|
            Decode.map tagger <|
                Decode.map3
                    (\x y z -> toFloat (x - y) / toFloat z)
                    (Decode.at [ "offsetX" ] Decode.int)
                    (Decode.at [ "target", "offsetLeft" ] Decode.int)
                    (Decode.at [ "target", "offsetWidth" ] Decode.int)


played : Model -> Element msg
played model =
    let
        played =
            model.state.currentTime

        unplayed =
            model.state.duration - played
    in
    row
        [ width fill
        , height fill
        ]
        [ el [ width (fillPortion <| floor played), height fill, Background.color Color.blue ] empty
        , el [ width (fillPortion <| floor unplayed), height fill, transparent True ] empty
        ]


timeToString : Float -> String
timeToString time =
    let
        timeDigits : Int -> String
        timeDigits v =
            case v <= 9 of
                True ->
                    "0" ++ toString v

                False ->
                    toString v

        h =
            floor time // 3600

        m =
            rem (floor time) 3600 // 60

        s =
            rem (rem (floor time) 3600) 60
    in
    case h <= 0 of
        False ->
            timeDigits h ++ ":" ++ timeDigits m ++ ":" ++ timeDigits s

        True ->
            timeDigits m ++ ":" ++ timeDigits s


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes Resize



-- SVG Icons


playIcon : String -> Html Msg
playIcon color =
    svg [ SvgAttr.viewBox "0 0 36 36", Svg.Events.onClick Play ]
        [ Svg.polygon [ SvgAttr.fill color, SvgAttr.points "4 0 4 36 36 18" ] [] ]


pauseIcon : String -> Html msg
pauseIcon color =
    Svg.svg [ SvgAttr.viewBox "0 0 36 36" ]
        [ Svg.g [ SvgAttr.fill color ]
            [ Svg.rect [ SvgAttr.x "4", SvgAttr.y "0", SvgAttr.rx "3", SvgAttr.width "12", SvgAttr.height "36" ] []
            , Svg.rect [ SvgAttr.x "22", SvgAttr.y "0", SvgAttr.rx "3", SvgAttr.width "12", SvgAttr.height "36" ] []
            ]
        ]


forwardIcon : String -> Html msg
forwardIcon color =
    svg [ SvgAttr.viewBox "0 0 36 36" ]
        [ Svg.circle
            [ SvgAttr.cx "18"
            , SvgAttr.cy "18"
            , SvgAttr.r "15"
            , SvgAttr.stroke color
            , SvgAttr.strokeWidth "2"
            , SvgAttr.fill "none"
            , SvgAttr.strokeDashoffset "3"
            , SvgAttr.strokeDasharray "75, 25"
            ]
            []
        , Svg.polygon [ SvgAttr.fill color, SvgAttr.points "18 6 18 0 22 3" ] []
        , Svg.text_ [ SvgAttr.x "18", SvgAttr.y "22", SvgAttr.fontSize "12", SvgAttr.textAnchor "middle" ] [ Svg.text "30s" ]
        ]


backwardIcon : String -> Html msg
backwardIcon color =
    svg
        [ SvgAttr.viewBox "0 0 36 36"

        {--, Svg.Events.onClick <| Seek (model.currentTime - 30)--}
        ]
        [ Svg.circle
            [ SvgAttr.cx "18"
            , SvgAttr.cy "18"
            , SvgAttr.r "15"
            , SvgAttr.stroke color
            , SvgAttr.strokeWidth "2"
            , SvgAttr.fill "none"
            , SvgAttr.strokeDashoffset "30"
            , SvgAttr.strokeDasharray "75, 25"
            ]
            []
        , Svg.polygon [ SvgAttr.fill color, SvgAttr.points "14 3 18 0 18 6" ] []
        , Svg.text_ [ SvgAttr.x "18", SvgAttr.y "22", SvgAttr.fontSize "12", SvgAttr.textAnchor "middle" ] [ Svg.text "30s" ]
        ]
