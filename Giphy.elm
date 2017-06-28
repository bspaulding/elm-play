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
    }


init =
    ( Model "cats" "waiting.gif" True, getRandomGif "cats" )


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)


decodeGifUrl =
    Decode.at [ "data", "image_url" ] Decode.string


getRandomGif topic =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic

        request =
            Http.get url decodeGifUrl
    in
        Http.send NewGif request


update msg model =
    case msg of
        MorePlease ->
            ( { model | loading = True }, getRandomGif model.topic )

        NewGif (Ok url) ->
            ( { model | loading = False, gifUrl = url }, Cmd.none )

        NewGif (Err _) ->
            ( { model | loading = False }, Cmd.none )


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


view model =
    div []
        [ h2 [] [ text model.topic ]
        , span
            [ style
                [ ( "display", loadingDisplay model.loading ) ]
            ]
            [ text "Loading..." ]
        , img [ src model.gifUrl ] []
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
