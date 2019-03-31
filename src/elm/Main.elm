module Main exposing (main)

--import Html exposing (Html, text, node, div, header, section, nav, footer, h1, h2, p, a, ul, li, img)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Time


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { userState : UserState
    }


type UserState
    = Init
    | Loaded Vehicles
    | Failed Http.Error


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Init
    , fetchJson
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | Recieve (Result Http.Error Vehicles)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( model, fetchJson )

        Recieve (Ok vehicles) ->
            ( { model | userState = Loaded vehicles }, Cmd.none )

        Recieve (Err error) ->
            ( { model | userState = Failed error }, Cmd.none )


fetchJson : Cmd Msg
fetchJson =
    Http.get
        { url = "https://m.nascar.com/live/feeds/series_1/4779/live_feed.json"
        , expect = Http.expectJson Recieve userDecoder
        }



-- HTTP


type alias Vehicle =
    { runningPosition : Int
    , vehicleNumber : String
    , fullName : String
    , vehicleManufacturer : String
    , lapsCompleted : Int
    , delta : Float
    , last_lap_time : Float
    , last_lap_speed : Float
    , pitStops : List Stop
    }


type alias Vehicles =
    List Vehicle


type alias Stop =
    { pit_in_lap_count : Int
    }


userDecoder : Decode.Decoder Vehicles
userDecoder =
    Decode.field "vehicles" (Decode.list vehicle)


vehicle : Decode.Decoder Vehicle
vehicle =
    Decode.succeed Vehicle
        |> required "running_position" Decode.int
        |> required "vehicle_number" Decode.string
        |> required "driver" (Decode.field "full_name" Decode.string)
        |> required "vehicle_manufacturer" Decode.string
        |> required "laps_completed" Decode.int
        |> required "delta" Decode.float
        |> required "last_lap_time" Decode.float
        |> required "last_lap_speed" Decode.float
        |> required "pit_stops" (Decode.list stop)


stop : Decode.Decoder Stop
stop =
    Decode.succeed Stop
        |> required "pit_in_lap_count" Decode.int



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 5000 Tick



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Leaderboard - NASCAR"
    , body =
        [ siteHeader
        , node "main"
            []
            [ section []
                [ case model.userState of
                    Init ->
                        text ""

                    Loaded vehicles ->
                        table [ class "leaderboard" ]
                            [ tr []
                                [ th [] [ text "Pos" ]
                                , th [] [ text "#" ]
                                , th [] [ text "Driver" ]
                                , th [] [ text "" ]
                                , th [] [ text "Laps" ]
                                , th [] [ text "Delta" ]
                                , th [] [ text "Last Lap" ]
                                , th [] [ text "mph" ]
                                , th [] [ text "Pit Stops" ]
                                , th [] [ text "Last Pit" ]
                                ]
                            , tbody [] (List.map viewRaces vehicles)
                            ]

                    Failed error ->
                        div [] [ text (Debug.toString error) ]
                ]
            ]
        , siteFooter
        ]
    }


viewRaces d =
    tr []
        [ td [] [ text (String.fromInt d.runningPosition) ]
        , td [] [ text d.vehicleNumber ]
        , td [] [ text d.fullName ]
        , td []
            [ case d.vehicleManufacturer of
                "Chv" ->
                    text "Chevrolet"

                "Frd" ->
                    text "Ford"

                "Tyt" ->
                    text "Toyota"

                _ ->
                    text ""
            ]
        , td [] [ text (String.fromInt d.lapsCompleted) ]
        , td [] [ text (String.fromFloat d.delta) ]
        , td [] [ text (String.fromFloat d.last_lap_time) ]
        , td [] [ text (String.fromFloat d.last_lap_speed) ]
        , td [] [ text (String.fromInt (List.length d.pitStops - 1)) ]
        , td []
            [ let
                lastStop =
                    d.pitStops |> List.reverse |> List.head |> Maybe.withDefault { pit_in_lap_count = 0 }
              in
              text (String.fromInt lastStop.pit_in_lap_count)
            ]
        ]


siteHeader : Html Msg
siteHeader =
    Html.header [ class "site-header" ]
        [ h1 [] [ text "Leaderboard" ]
        ]


siteFooter : Html Msg
siteFooter =
    footer [ class "site-footer" ]
        [ p [ class "copyright" ]
            [ text "© 2019 "
            , a [ href "https://y047aka.me", target "_blank" ] [ text "y047aka" ]
            ]
        ]
