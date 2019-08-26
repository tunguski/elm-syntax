module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Browser
import Elm.Internal.RawFile
import Elm.Parser
import Elm.Processing
import Elm.RawFile as Elm
import Elm.Syntax.File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as JD
import Parser exposing (DeadEnd)
import Platform.Sub


type Msg
    = Replace String String String
    | Loaded String String (Result Error String)


type alias Item =
    { moduleName : String
    , text : Maybe String
    , parsed :
        Maybe (Result (List DeadEnd) Elm.Internal.RawFile.RawFile)
    }


type alias Package =
    { package : String
    , items : List Item
    }


type alias Model =
    List Package


testFiles =
    [ ( "elm/core/1.0.2/src/"
      , [ "Array"
        , "Basics"
        , "Bitwise"
        , "Char"
        , "Debug"
        , "Dict"
        , "List"
        , "Maybe"
        , "Platform"
        , "Platform/Cmd"
        , "Platform/Sub"
        , "Process"
        , "Result"
        , "Set"
        , "String"
        , "Task"
        , "Tuple"
        ]
      )
    , ( "elm/browser/1.0.1/src/"
      , [ "Browser"
        , "Browser/Dom"
        , "Browser/Events"
        , "Browser/Navigation"
        ]
      )
    , ( "elm/bytes/1.0.8/src/"
      , [ "Bytes"
        , "Bytes/Decode"
        , "Bytes/Encode"
        ]
      )
    , ( "elm/json/1.1.3/src/"
      , [ "Json/Decode"
        , "Json/Encode"
        ]
      )
    , ( "elm/http/2.0.0/src/"
      , [ "Http"
        ]
      )
    , ( "elm/html/1.0.0/src/"
      , [ "Html"
        , "Html/Attributes"
        , "Html/Events"
        , "Html/Keyed"
        , "Html/Lazy"
        ]
      )
    ]


init : () -> ( Model, Cmd Msg )
init x =
    ( Package "n/a"
        [ Item "Custom Editor"
            (Just sampleModule)
            (Just <| Elm.Parser.parse sampleModule)
        ]
        :: List.map
            (\( pkg, items ) ->
                Package pkg <|
                    List.map
                        (\moduleName ->
                            Item moduleName Nothing Nothing
                        )
                        items
            )
            testFiles
    , List.concatMap
        (\( package, files ) ->
            List.map
                (\file ->
                    getString
                        package
                        file
                        ("https://raw.githubusercontent.com/" ++ package ++ file ++ ".elm")
                )
                files
        )
        testFiles
        |> Cmd.batch
    )


getString package file url =
    Http.get
        { url = url
        , expect = Http.expectString (Loaded package file)
        }


sampleModule =
    """module Main exposing (..)

f : Int -> Int
f x = x + 1

g : Int -> Int
g x = x * 2

h = f << g


update : Msg -> String -> String
update action model =
    case action of
        Replace m ->
            m


withChild : a -> List (Html Msg) -> Html Msg
withChild title children =
    li []
        [ pre [] [ text <| Debug.toString title ]
        , ul [] children
        ]
"""


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Replace pkg name m ->
            ( List.map
                (\package ->
                    if package.package == pkg then
                        { package
                            | items =
                                List.map
                                    (\item ->
                                        if item.moduleName == name then
                                            { item
                                                | text = Just m
                                                , parsed = Just (Elm.Parser.parse m)
                                            }

                                        else
                                            item
                                    )
                                    package.items
                        }

                    else
                        package
                )
                model
            , Cmd.none
            )

        Loaded pkg name result ->
            case result of
                Ok data ->
                    ( List.map
                        (\package ->
                            if package.package == pkg then
                                { package
                                    | items =
                                        List.map
                                            (\item ->
                                                if item.moduleName == name then
                                                    { item
                                                        | text = Just data
                                                        , parsed = Just (Elm.Parser.parse data)
                                                    }

                                                else
                                                    item
                                            )
                                            package.items
                                }

                            else
                                package
                        )
                        model
                    , Cmd.none
                    )

                Err err ->
                    let
                        x =
                            Debug.log "error" err
                    in
                    ( model, Cmd.none )


withChild : a -> List (Html Msg) -> Html Msg
withChild title children =
    li []
        [ text <| Debug.toString title
        , ul [] children
        ]


tree ast =
    text <| Debug.toString ast


countItems : Bool -> Model -> Int
countItems value model =
    model
        |> List.map .items
        |> List.concatMap identity
        |> List.filter
            (\i ->
                case i.parsed of
                    Just ast ->
                        case ast of
                            Ok _ ->
                                value

                            _ ->
                                not value

                    _ ->
                        False
            )
        |> List.length


view : Model -> Html Msg
view model =
    Grid.containerFluid [] <|
        [ CDN.stylesheet

        --, navbar model
        ]
            ++ mainContent model


mainContent model =
    [ Grid.simpleRow
        [ Grid.col
            [ Col.xs12 ]
            [ h1 []
                [ text "All items: "
                , text <|
                    Debug.toString
                        (model
                            |> List.map .items
                            |> List.concatMap identity
                            |> List.length
                        )
                , text " Success count: "
                , text <| Debug.toString <| countItems True model
                , text " Failure count: "
                , text <| Debug.toString <| countItems False model
                ]
            ]
        ]
    ]
        ++ List.concatMap identity
            (List.map
                (\package ->
                    [ Grid.simpleRow
                        [ Grid.col [ Col.xs12 ]
                            [ h2 [] [ text <| "Package: " ++ package.package ] ]
                        ]
                    ]
                        ++ List.concatMap identity
                            (List.map
                                (\item ->
                                    [ Grid.simpleRow
                                        [ Grid.col [ Col.xs12 ]
                                            [ h2 [] [ text <| "Module: " ++ item.moduleName ] ]
                                        ]
                                    , Grid.simpleRow
                                        [ Grid.col
                                            [ Col.xs4 ]
                                            [ textarea
                                                [ on "input" (JD.map (Replace package.package item.moduleName) targetValue)
                                                , style "width" "100%"
                                                , style "padding" "0"
                                                , style "position" "absolute"
                                                , style "top" "0"
                                                , style "bottom" "0"
                                                , style "left" "0"
                                                , style "right" "0"
                                                ]
                                                [ text <|
                                                    case item.text of
                                                        Just txt ->
                                                            txt

                                                        _ ->
                                                            ""
                                                ]
                                            ]
                                        , Grid.col
                                            ((Col.attrs <|
                                                List.singleton <|
                                                    class <|
                                                        case item.parsed of
                                                            Just ast ->
                                                                case ast of
                                                                    Ok _ ->
                                                                        "card bg-success text-white"

                                                                    _ ->
                                                                        "card bg-danger text-white"

                                                            _ ->
                                                                ""
                                             )
                                                :: [ Col.xs8
                                                   ]
                                            )
                                            [ case item.parsed of
                                                Just ast ->
                                                    tree ast

                                                _ ->
                                                    text ""
                                            ]
                                        ]
                                    ]
                                )
                                package.items
                            )
                )
                model
            )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \m -> Sub.none
        }
