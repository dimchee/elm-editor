module Main exposing (..)

import Browser
import Browser.Dom
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Json.Decode as JD
import Task


-- TODO optimization, make lines [ Str String | Blank Int ]
-- TODO maybe even just one big buffer, and save line sizes
type alias Model =
    { lines : Text Char
    , cursor : Cursor
    }


type alias Position =
    { line : Int
    , column : Int
    }


type Direction
    = Horizontal Int
    | Vertical Int


type alias Cursor =
    Int


type Msg
    = NoOp
    | Move Direction
    | Insert Char


init : Model
init =
    { lines = toText [ "line1", "line2", "line3 aaa" ]
    , cursor = 1
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
                    , viewDebug model
                    ]
                }
        , subscriptions = \_ -> Sub.none
        }


viewDebug : Model -> Html Msg
viewDebug { lines, cursor } =
    Html.text <| Debug.toString <| cursorToPos lines cursor


type alias TextChar char =
    Maybe char


type alias TextLine char =
    { start : Int
    , line : List (TextChar char)
    }


type alias Text char =
    { text : List (TextLine char)
    , len : Int
    }


type alias TextHtml =
    Text (Html Msg)


toText : List String -> Text Char
toText =
    List.foldl
        (\line { text, len } ->
            let
                newline =
                    (line |> String.toList |> List.map Just)
                        ++ [ Nothing ]
            in
            { text =
                { start = len
                , line = newline
                }
                    :: text
            , len = len + List.length newline
            }
        )
        { text = [], len = 0 }
        >> mapLines List.reverse


viewText : TextHtml -> List (Html Msg)
viewText =
    .text >> List.map (.line >> List.filterMap identity >> Html.div [])


mapLines : (List (TextLine a) -> List (TextLine b)) -> Text a -> Text b
mapLines f { text, len } =
    { text = f text, len = len }


indexedMap : (Int -> TextChar a -> TextChar b) -> Text a -> Text b
indexedMap f =
    mapLines <|
        List.map
            (\{ start, line } ->
                { start = start
                , line =
                    line
                        |> List.indexedMap (\i -> f (start + i))
                }
            )


addCursor : Cursor -> TextHtml -> TextHtml
addCursor cursor =
    indexedMap
        (\ind ->
            if ind == cursor then
                Maybe.withDefault (Html.text "\u{00A0}")
                    >> List.singleton
                    >> Html.span
                        [ HA.style "background-color" "orange"
                        ]
                    >> Just

            else
                identity
        )


viewNumbers : List a -> List (Html Msg)
viewNumbers =
    List.indexedMap (\num _ -> Html.div [] <| List.singleton <| Html.text <| String.fromInt num)


listPrepend : a -> List a -> List a
listPrepend x xs =
    xs ++ [ x ]


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
        [ viewNumbers lines.text
            |> Html.div
                [ HA.style "display" "flex"
                , HA.style "flex-direction" "column"
                , HA.style "width" "2em"
                , HA.style "text-align" "center"
                , HA.style "color" "#888"
                ]
        , lines
            |> indexedMap (\_ -> Maybe.map (String.fromChar >> Html.text))
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
            JD.succeed <| Move <| Vertical -1

        "ArrowDown" ->
            JD.succeed <| Move <| Vertical  1

        "ArrowRight" ->
            JD.succeed <| Move <| Horizontal 1

        "ArrowLeft" ->
            JD.succeed <| Move <| Horizontal -1

        _ ->
            case string |> String.toList of
                [ char ] ->
                    JD.succeed <| Insert char

                _ ->
                    JD.fail "Key not bound"


cursorToPos : Text a -> Cursor -> Maybe Position
cursorToPos { text } cursor =
    text
        |> List.indexedMap (\line { start } -> ( line, start ))
        |> List.filter (\( _, start ) -> start <= cursor)
        |> List.reverse
        |> List.head
        |> Maybe.map (\( line, start ) -> { line = line, column = cursor - start })


posToCursor : Text a -> Position -> Maybe Cursor
posToCursor { text } pos =
    let
        cursorLine =
            clamp 0 (List.length text - 1) pos.line
    in
    text
        |> List.indexedMap (\lineNum { start, line } -> ( lineNum, start, List.length line ))
        |> Debug.log "        [ ( lineNum, start, lineLen ) ]: "
        |> List.filter (\( line, _, _ ) -> line == cursorLine)
        |> Debug.log "        filtered: "
        |> List.head
        |> Debug.log "        first: "
        |> Maybe.map (\( _, start, lineLen ) -> start + clamp 0 (lineLen - 1) pos.column)


moveCursor : Direction -> Text Char -> Cursor -> Cursor
moveCursor dir text cursor =
    clamp 0 (text.len - 1) <|
        case dir of
            Horizontal delta ->
                cursor + delta

            Vertical delta ->
                cursor
                    |> Debug.log "cursor: "
                    |> cursorToPos text
                    |> Debug.log "    position: "
                    |> Maybe.map (\{ line, column } -> { line = line + delta, column = column })
                    |> Debug.log "    changed: "
                    |> Maybe.andThen (posToCursor text)
                    |> Debug.log "    backToCursor: "
                    |> Maybe.withDefault 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move dir ->
            ( { model | cursor = moveCursor dir model.lines model.cursor }, Cmd.none )
        Insert char -> ( addChar char model, Cmd.none )

        _ ->
            ( model, Cmd.none )


addChar : Char -> Model -> Model
addChar char { lines, cursor } =
    Debug.todo "TODO"

