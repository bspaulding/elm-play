module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Http
import Json.Decode as Decode
import List.Extra exposing (elemIndex, swapAt)
import Random
import Random.List exposing (shuffle)


type alias Model =
    { topic : String
    , imageUrl : String
    , loading : Bool
    , error : Maybe Http.Error
    , tiles : List Int
    , reveal : Bool
    }


appId =
    "5271e4f92d2b2f215bd33a876f3d5ccd9d05b8253920f4e8cc17bcb2e1a542c8"


appSecret =
    "1f7d52fa8d4d079ae908bbb61ad4f588779ba98ee5faa2d64b706d9bbace106c"


initialTiles =
    List.range 0 15


init =
    ( Model "coffee" "waiting.gif" True Nothing initialTiles False, getImage "coffee" )


type Msg
    = FetchNewImage
    | NewGif (Result Http.Error String)
    | UpdateTopic String
    | MoveTile Int
    | ShuffleTiles
    | ShuffledTiles (List Int)
    | Reveal
    | UnReveal


decodeImageUrl =
    Decode.at [ "urls", "regular" ] Decode.string


getImage topic =
    let
        url =
            "https://api.unsplash.com/photos/random?client_id=" ++ appId ++ "&query=" ++ topic

        request =
            Http.get url decodeImageUrl
    in
        Http.send NewGif request


moveTile tiles i =
    let
        i0 =
            elemIndex 0 tiles
    in
        case i0 of
            Just i0 ->
                case swapAt i0 i tiles of
                    Just swapped ->
                        swapped

                    Nothing ->
                        tiles

            Nothing ->
                tiles


validMove : List Int -> Int -> Bool
validMove tiles i =
    let
        i0 =
            elemIndex 0 tiles
    in
        case i0 of
            Just i0 ->
                i0 == i - 1 || i0 == i + 1 || i0 == i + 4 || i0 == i - 4

            Nothing ->
                False


update msg model =
    case msg of
        FetchNewImage ->
            ( { model | loading = True }, getImage model.topic )

        NewGif (Ok url) ->
            ( { model | loading = False, imageUrl = url }, Cmd.none )

        NewGif (Err error) ->
            ( { model | error = Just error, loading = False }, Cmd.none )

        UpdateTopic topic ->
            ( { model | loading = True, topic = topic }, getImage topic )

        MoveTile i ->
            if validMove model.tiles i then
                ( { model | tiles = moveTile model.tiles i }, Cmd.none )
            else
                ( model, Cmd.none )

        ShuffleTiles ->
            ( model, Random.generate ShuffledTiles (shuffle initialTiles) )

        ShuffledTiles tiles ->
            ( { model | tiles = tiles }, Cmd.none )

        Reveal ->
            ( { model | reveal = True }, Cmd.none )

        UnReveal ->
            ( { model | reveal = False }, Cmd.none )


withMeta html =
    div []
        [ node "meta" [ name "viewport", content "width=device-width,initial-scale=1,maximum-scale=1,user-scalable=no" ] []
        , div
            [ style
                [ ( "position", "absolute" )
                , ( "top", "0px" )
                , ( "left", "0px" )
                , ( "right", "0px" )
                , ( "bottom", "0px" )
                ]
            ]
            [ html ]
        ]


loadingDisplay loading =
    if loading then
        "block"
    else
        "none"


errorMessage error =
    case error of
        Just e ->
            case e of
                Http.BadUrl _ ->
                    "bad url"

                Http.Timeout ->
                    "timeout"

                Http.NetworkError ->
                    "network error"

                Http.BadStatus response ->
                    "bad status" ++ response.body

                Http.BadPayload _ _ ->
                    "bad payload"

        Nothing ->
            ""


topics =
    [ "beach", "clouds", "coffee", "trees" ]


makeTopicOption currentTopic topic =
    option [ selected <| currentTopic == topic ] [ text topic ]


tileCell : Model -> Int -> Int -> Html Msg
tileCell model i t =
    let
        col =
            i % 4

        row =
            floor <| (toFloat i) / 4

        x =
            col * 100

        y =
            row * 100
    in
        if t > 0 then
            div
                [ style
                    [ ( "width", "100px" )
                    , ( "height", "100px" )
                    , ( "position", "absolute" )
                    , ( "overflow", "hidden" )
                    , ( "top", (toString y) ++ "px" )
                    , ( "left", (toString x) ++ "px" )
                    , ( "-webkit-transition", "top 0.3s, left 0.3s" )
                    , ( "transition", "top 0.3s, left 0.3s" )
                    ]
                , onClick (MoveTile i)
                , id <| "tile-" ++ (toString t)
                ]
                [ tileImage model t ]
        else
            text ""


keyedTileCell : Model -> Int -> Int -> ( String, Html Msg )
keyedTileCell model i t =
    ( "tile-" ++ (toString t), tileCell model i t )


tileImage : Model -> Int -> Html msg
tileImage model t =
    let
        col =
            t % 4

        row =
            floor <| (toFloat t) / 4

        x =
            col * -100

        y =
            row * -100
    in
        img
            [ src model.imageUrl
            , style
                [ ( "position", "absolute" )
                , ( "top", (toString y) ++ "px" )
                , ( "left", (toString x) ++ "px" )
                , ( "height", "400px" )
                ]
            ]
            []


tileGrid model =
    let
        tiles =
            if model.reveal then
                initialTiles
            else
                model.tiles
    in
        Html.Keyed.node "div"
            [ style
                [ ( "position", "relative" )
                , ( "width", "400px" )
                , ( "height", "400px" )
                ]
            ]
        <|
            List.sortBy Tuple.first <|
                List.indexedMap (keyedTileCell model) tiles


onTouchStart message =
    on "touchstart" <| Decode.succeed message


onTouchEnd message =
    on "touchend" <| Decode.succeed message


view model =
    div
        [ style
            [ ( "display", "flex" )
            , ( "flexDirection", "column" )
            , ( "alignItems", "center" )
            ]
        ]
        [ h2 [] [ text model.topic ]
        , select [ onInput UpdateTopic ]
            (List.map
                (makeTopicOption model.topic)
                topics
            )
        , span [] [ text <| errorMessage model.error ]
        , span
            [ style
                [ ( "display", loadingDisplay model.loading ) ]
            ]
            [ text "Loading..." ]
        , tileGrid model
        , button [ onClick FetchNewImage ] [ text "Next Image" ]
        , button [ onClick ShuffleTiles ] [ text "Shuffle" ]
        , button
            [ onMouseDown Reveal
            , onMouseUp UnReveal
            , onTouchStart Reveal
            , onTouchEnd UnReveal
            , style
                [ ( "user-select", "none" )
                , ( "-webkit-user-select", "none" )
                , ( "-moz-user-select", "none" )
                , ( "-ms-user-select", "none" )
                ]
            ]
            [ text "Reveal" ]
        ]
        |> withMeta


subscriptions model =
    Sub.none


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
