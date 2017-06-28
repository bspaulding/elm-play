module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


type alias Model =
    { topic : String
    , gifUrl : String
    , loading : Bool
    , error : Maybe Http.Error
    }


appId =
    "5271e4f92d2b2f215bd33a876f3d5ccd9d05b8253920f4e8cc17bcb2e1a542c8"


appSecret =
    "1f7d52fa8d4d079ae908bbb61ad4f588779ba98ee5faa2d64b706d9bbace106c"


init =
    ( Model "coffee" "waiting.gif" True Nothing, getImage "coffee" )


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)
    | UpdateTopic String


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


update msg model =
    case msg of
        MorePlease ->
            ( { model | loading = True }, getImage model.topic )

        NewGif (Ok url) ->
            ( { model | loading = False, gifUrl = url }, Cmd.none )

        NewGif (Err error) ->
            ( { model | error = Just error, loading = False }, Cmd.none )

        UpdateTopic topic ->
            ( { model | loading = True, topic = topic }, getImage topic )


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
        , img
            [ src model.gifUrl
            , style [ ( "width", "100%" ) ]
            ]
            []
        , button [ onClick MorePlease ] [ text "More Please" ]
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
