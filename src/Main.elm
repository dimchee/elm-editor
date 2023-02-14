module Main exposing (..)

import Browser
import Browser.Dom
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Json.Decode as JD
import List.Extra
import Task


type alias Position =
    Int


type alias Model =
    { buffer : List Token
    , cursor : Position
    }


type Direction
    = Horizontal Int
    | Vertical Int


type Msg
    = NoOp
    | Move Direction
    | Insert Char


init : Model
init =
    { buffer = toTokens [ "line1", "line2", "line3 aaa", "line4" ]
    , cursor = 11
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ buffer, cursor } as model) =
    case msg of
        Move dir ->
            ( { model | cursor = moveCursor dir buffer cursor }, Cmd.none )

        -- Insert char ->
        --     ( { model
        --         | buffer =
        --             List.take (toIndex buffer cursor) buffer
        --                 ++ (Str [] <| String.fromChar char)
        --                 :: List.drop (toIndex buffer cursor) buffer
        --       }
        --     , Cmd.none
        --     )
        _ ->
            ( model, Cmd.none )


viewDebug : Model -> Html msg
viewDebug { buffer, cursor } =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ Html.div [] [ Html.text <| Debug.toString <| lineLengths buffer ]
        , Html.div [] [ Html.text <| Debug.toString <| toVisPos buffer cursor ]
        , Html.div []
            [ Html.text <| Debug.toString <| toIndex buffer <| toVisPos buffer cursor
            ]

        -- , Html.div [] [ Html.text <| Debug.toString <| buffer ]
        , Html.div [] [ Html.text <| Debug.toString <| tokensToRanges buffer ]
        ]


viewEditor : Model -> Html Msg
viewEditor { buffer, cursor } =
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
        [ buffer
            |> List.filter (\x -> x == NewLine)
            |> List.indexedMap (\num _ -> Html.div [] <| List.singleton <| Html.text <| String.fromInt num)
            |> Html.div
                [ HA.style "display" "flex"
                , HA.style "flex-direction" "column"
                , HA.style "width" "2em"
                , HA.style "text-align" "center"
                , HA.style "color" "#888"
                ]
        , buffer
            |> addCursor cursor
            |> tokensToHtml
            |> List.map (Html.div [])
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
            JD.succeed <| Move <| Vertical 1

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


type alias Range =
    { from : Position
    , to : Position
    }


tokensToRanges : List Token -> List Range
tokensToRanges =
    List.map (\tok -> ( 0, length tok - 1 ))
        >> List.Extra.scanl1 (\( _, len ) ( _, end ) -> ( end + 1, len + 1 |> (+) end ))
        >> List.map (\( start, end ) -> { from = start, to = end })


mapRange : Range -> (Token -> Token) -> List Token -> List Token
mapRange target f tokens =
    List.map2 (\x y -> ( [ x ], y )) tokens (tokensToRanges tokens)
        |> List.Extra.updateIf (\( _, { from, to } ) -> from <= target.from && target.to <= to)
            (\( toks, { from, to } ) ->
                ( case toks of
                    [ Str ts str ] ->
                        [ Str ts <| String.slice 0 (target.from - from) str
                        , f <| Str ts <| String.slice (target.from - from) (target.to - from + 1) str
                        , Str ts <| String.slice (target.to - from + 1) (to - from + 1) str
                        ]

                    [ NewLine ] ->
                        [ f <| NewLine, NewLine ]

                    _ ->
                        -- unreachable
                        toks
                , { from = from, to = to }
                )
            )
        |> List.concatMap Tuple.first


addCursor : Position -> List Token -> List Token
addCursor cursor =
    let
        cursorTransformer =
            List.singleton
                >> Html.span
                    [ HA.style "background-color" "orange"
                    ]
    in
    mapRange { from = cursor, to = cursor }
        (\token ->
            case token |> Debug.log "Tok: " of
                Str ts str ->
                    Str (cursorTransformer :: ts) <| str

                _ ->
                    Str [ cursorTransformer ] "\u{00A0}"
        )


toMaybe : Token -> Maybe ( List Transformer, String )
toMaybe tok =
    case tok of
        Str ts str ->
            Just <| ( ts, str )

        _ ->
            Nothing


toLines : List Token -> List ( Token, List Token )
toLines =
    List.Extra.groupWhile (\tok _ -> tok /= NewLine)


lineLengths : List Token -> List Int
lineLengths =
    List.Extra.groupWhile (\tok _ -> tok /= NewLine)
        >> List.map ((\( a, b ) -> a :: b) >> List.map length >> List.sum)


toIndex : List Token -> { line : Int, column : Int } -> Int
toIndex toks { line, column } =
    toks
        |> lineLengths
        |> List.take (line + 1)
        |> List.Extra.unconsLast
        |> Maybe.map (\( x, xs ) -> List.sum xs + min column (x - 1))
        |> Maybe.withDefault column


toVisPos : List Token -> Int -> { line : Int, column : Int }
toVisPos toks pos =
    toks
        |> lineLengths
        |> List.Extra.scanl1 (+)
        |> List.Extra.takeWhile ((>=) pos)
        |> (\l -> { line = List.length l, column = List.Extra.last l |> Maybe.withDefault 0 |> (-) pos })


tokensToHtml : List Token -> List (List (Html Msg))
tokensToHtml =
    toLines
        >> List.map (\( tok, toks ) -> tok :: toks)
        >> List.map (List.filterMap toMaybe >> List.map transform)


moveCursor : Direction -> List Token -> Position -> Position
moveCursor dir tokens cursor =
    case dir of
        Horizontal delta ->
            cursor + delta |> clamp 0 ((lineLengths tokens |> List.sum) - 1)

        Vertical delta ->
            cursor
                |> toVisPos tokens
                |> (\{ line, column } -> { line = line + delta, column = column })
                |> toIndex tokens


type alias Transformer =
    Html Msg -> Html Msg


type Token
    = Str (List Transformer) String
    | NewLine


transform : ( List Transformer, String ) -> Html Msg
transform ( ts, str ) =
    List.foldl (\t acc -> t acc) (Html.text str) ts


length : Token -> Int
length tok =
    case tok of
        Str _ str ->
            String.length str

        _ ->
            1


toTokens : List String -> List Token
toTokens =
    List.concatMap (\line -> [ Str [] line, NewLine ])
