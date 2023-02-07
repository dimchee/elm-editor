module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Json.Decode as JD
import Task


type alias Model =
    { lines : Array String
    , cursor : Position
    }


type alias Position =
    { line : Int
    , column : Int
    }


type alias Direction =
    Position


direction : ( Int, Int ) -> Position
direction ( x, y ) =
    { line = y, column = x }


type Msg
    = NoOp
    | Move Direction
    | Insert Char


init : Model
init =
    { lines = Array.fromList [ "line1", "line2", "line3 aaa" ]
    , cursor = { line = 0, column = 5 }
    }


main : Program () Model Msg
main =
    Browser.document
        { init =
            \() ->
                ( init
                , Browser.Dom.focus "editor"
                    |> Task.attempt (always NoOp)
                )
        , update = update
        , view =
            \model ->
                { title = "elm-editor"
                , body =
                    [ viewEditor model

                    -- , Editor.viewDebug model
                    ]
                }
        , subscriptions = \_ -> Sub.none
        }


type alias TextChar =
    Html Msg


type alias TextLine =
    List TextChar


type alias Text =
    List TextLine


toText : Array String -> Text
toText =
    Array.toList >> List.map (String.toList >> List.map (String.fromChar >> Html.text))


viewText : Text -> List (Html Msg)
viewText =
    List.map (Html.div [])


onEqTransform : a -> a -> obj -> (obj -> obj) -> obj
onEqTransform x y obj f =
    if x == y then
        f obj

    else
        obj


viewCursor : Position -> Int -> Int -> TextChar -> TextChar
viewCursor cursor row col ch =
    onEqTransform cursor { line = row, column = col } ch <|
        (List.singleton
            >> Html.span
                [ HA.style "background-color" "orange"
                ]
        )


textAppend : Html Msg -> TextLine -> TextLine
textAppend el l =
    l ++ [ el ]


addCursor : Position -> Text -> Text
addCursor cursor =
    List.indexedMap
        (\ind line ->
            textAppend (Html.text "\u{00A0}")
                |> onEqTransform cursor { line = ind, column = List.length line } line
        )
        >> List.indexedMap (\row -> List.indexedMap (viewCursor cursor row))


viewNumbers : Array String -> List (Html Msg)
viewNumbers =
    Array.toIndexedList
        >> List.map (\( num, _ ) -> Html.div [] <| List.singleton <| Html.text <| String.fromInt num)


viewEditor : Model -> Html Msg
viewEditor { lines, cursor } =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "font-family" "monospace"
        , HA.style "font-size" "20px"
        , HA.style "tab-size" "4"
        , Html.Events.on "keydown"
            (JD.field "key" JD.string
                |> JD.andThen keyToMsg
            )
        , HA.tabindex 0 -- to be able to focus with click
        , HA.id "editor" -- for focusing
        ]
        [ viewNumbers lines
            |> Html.div
                [ HA.style "display" "flex"
                , HA.style "flex-direction" "column"
                , HA.style "width" "2em"
                , HA.style "text-align" "center"
                , HA.style "color" "#888"
                ]
        , lines
            |> toText
            |> addCursor cursor
            |> viewText
            |> Html.div
                [ HA.style "background-color" "#f0f0f0"
                , HA.style "width" "100%"
                ]
        ]


keyToMsg : String -> JD.Decoder Msg
keyToMsg string =
    case string of
        "Tab" ->
            JD.succeed <| Insert '\t'

        "ArrowUp" ->
            JD.succeed <| Move <| direction ( 0, -1 )

        "ArrowDown" ->
            JD.succeed <| Move <| direction ( 0, 1 )

        "ArrowLeft" ->
            JD.succeed <| Move <| direction ( -1, 0 )

        "ArrowRight" ->
            JD.succeed <| Move <| direction ( 1, 0 )

        _ ->
            case string |> String.toList of
                [ char ] ->
                    JD.succeed <| Insert char

                _ ->
                    JD.fail "Key not bound"


clampCursor : ( Int, Int ) -> Position -> Position
clampCursor ( ln, col ) { line, column } =
    { line = clamp 0 (ln - 1) line, column = clamp 0 col column }


snapToGrid : { a | lines : Array String, cursor : Position } -> { a | lines : Array String, cursor : Position }
snapToGrid ({ lines, cursor } as m) =
    { m
        | cursor =
            clampCursor
                ( Array.length lines
                , Array.get cursor.line lines
                    |> Maybe.map String.length
                    |> Maybe.withDefault cursor.column
                )
                cursor
    }


moveCursor : Direction -> Position -> Position
moveCursor dir { line, column } =
    { line = line + dir.line, column = column + dir.column }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move dir ->
            ( { model | cursor = moveCursor dir model.cursor } |> snapToGrid, Cmd.none )

        _ ->
            ( model, Cmd.none )
