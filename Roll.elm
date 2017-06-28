module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (..)


type alias Model =
    { dieFace : Int }


type Msg
    = Roll
    | NewFace Int


init =
    ( Model 1, Cmd.none )


update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace (Random.int 1 6) )

        NewFace i ->
            ( { model | dieFace = i }, Cmd.none )


subscriptions model =
    Sub.none


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


view model =
    div
        [ style
            [ ( "display", "flex" )
            , ( "alignItems", "center" )
            , ( "justifyContent", "center" )
            , ( "flexDirection", "column" )
            , ( "height", "100%" )
            ]
        ]
        [ h1 [] [ text (toString model.dieFace) ]
        , button [ onClick Roll ] [ text "Roll" ]
        ]
        |> withMeta


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
