module Main exposing (..)

import Browser
import Browser.Dom
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Json.Decode as JD
import List.Extra
import Maybe.Extra
import Task


type alias Position =
    Int


type alias Buffer =
    String


type Hover
    = NoHover
    | HoverLine Int
    | HoverChar Position


type Range
    = Empty Position
    | Inclusive
        { from : Position
        , to : Position
        }


type alias Model =
    { buffer : Buffer
    , cursor : Position
    , hover : Hover
    }


type Direction
    = Horizontal Int
    | Vertical Int


type Msg
    = NoOp
    | Move Direction
    | Insert String
    | Delete
    | DeleteWord
    | Hover Hover


toBuffer : List String -> Buffer
toBuffer =
    String.join "\n"


init : Model
init =
    { buffer = toBuffer [ "line1", "line2", "line3 aaa", "line4" ]
    , cursor = 2
    , hover = NoHover
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

        Insert str ->
            ( { model
                | buffer =
                    String.slice 0 cursor buffer
                        ++ str
                        ++ String.slice cursor (String.length buffer) buffer
                , cursor = cursor + 1
              }
            , Cmd.none
            )

        Delete ->
            ( if cursor > 0 then
                { model
                    | buffer =
                        String.slice 0 (cursor - 1) buffer
                            ++ String.slice cursor (String.length buffer) buffer
                    , cursor = cursor - 1
                }

              else
                model
            , Cmd.none
            )

        DeleteWord ->
            let
                wLen =
                    String.slice 0 (cursor - 1) buffer
                        |> String.words
                        |> List.Extra.last
                        |> Maybe.map String.length
                        |> Maybe.withDefault -1
                        |> (+) 1
            in
            ( if cursor > 0 then
                { model
                    | buffer =
                        String.slice 0 (cursor - wLen) buffer
                            ++ String.slice cursor (String.length buffer) buffer
                    , cursor = cursor - wLen
                }

              else
                model
            , Cmd.none
            )

        Hover hover ->
            ( { model
                | hover = hover
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


viewDebug : Model -> Html msg
viewDebug { buffer, cursor, hover } =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ Html.div [] [ Html.text <| Debug.toString <| lineLengths buffer ]
        , Html.div [] [ Html.text <| Debug.toString <| toVisPos buffer cursor ]
        , Html.div []
            [ Html.text <| Debug.toString <| toIndex buffer <| toVisPos buffer cursor
            ]
        , Html.div [] [ Html.text <| Debug.toString <| buffer ]
        , Html.div [] [ Html.text <| Debug.toString <| toTokens buffer ]
        , Html.div [] [ Html.text <| Debug.toString <| hover ]
        ]


viewEditor : Model -> Html Msg
viewEditor { buffer, cursor } =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "font-family" "monospace"
        , HA.style "font-size" "20px"
        , HA.style "tab-size" "4"
        , Html.Events.on "keydown"
            (JD.map2 Tuple.pair
                (JD.field "key" JD.string)
                (JD.field "ctrlKey" JD.bool)
                |> JD.andThen keyToMsg
            )
        , Html.Events.onMouseOut (Hover NoHover)
        , HA.tabindex 0 -- to be able to focus with click
        , HA.id "editor" -- for focusing
        ]
        [ buffer
            |> lineLengths
            |> List.indexedMap (\num _ -> Html.div [] <| List.singleton <| Html.text <| String.fromInt num)
            |> Html.div
                [ HA.style "display" "flex"
                , HA.style "flex-direction" "column"
                , HA.style "width" "2em"
                , HA.style "text-align" "center"
                , HA.style "color" "#888"
                ]
        , buffer
            |> toTokens
            |> addCursor cursor
            |> tokensToHtml
            |> List.indexedMap
                (\line ->
                    Html.div
                        [ HA.style "height" "24px"
                        , Html.Events.onMouseOver (Hover (HoverLine line))
                        ]
                )
            |> Html.div
                [ HA.style "background-color" "#f0f0f0"
                , HA.style "width" "100%"
                ]
        ]


keyToMsg : ( String, Bool ) -> JD.Decoder Msg
keyToMsg ( key, ctrl ) =
    case key of
        "ArrowUp" ->
            JD.succeed <| Move <| Vertical -1

        "ArrowDown" ->
            JD.succeed <| Move <| Vertical 1

        "ArrowRight" ->
            JD.succeed <| Move <| Horizontal 1

        "ArrowLeft" ->
            JD.succeed <| Move <| Horizontal -1

        "Tab" ->
            JD.succeed <| Insert "\t"

        "Enter" ->
            JD.succeed <| Insert "\n"

        "Backspace" ->
            if ctrl then
                JD.succeed <| DeleteWord

            else
                JD.succeed <| Delete

        _ ->
            case key |> String.toList of
                [ ' ' ] ->
                    JD.succeed <| Insert <| "\u{00A0}"

                [ char ] ->
                    JD.succeed <| Insert <| String.fromChar char

                _ ->
                    JD.fail "Key not bound"


getMiddle : Range -> String -> ( Maybe String, String, Maybe String )
getMiddle range str =
    let
        nonEmpty =
            Just >> Maybe.Extra.filter (not << String.isEmpty)
    in
    (\( l, m, r ) -> ( nonEmpty l, m, nonEmpty r )) <|
        case range of
            Inclusive { from, to } ->
                ( String.slice 0 from str, String.slice from (to + 1) str, String.dropLeft (to + 1) str )

            Empty pos ->
                ( String.slice 0 pos str, "", String.dropLeft pos str )


mapRange : Range -> (Token -> List Token) -> List Token -> List Token
mapRange target f =
    case target of
        Empty pos ->
            List.indexedMap
                (\ind tok ->
                    if ind == pos then
                        f tok

                    else
                        [ tok ]
                )
                >> List.concat

        Inclusive { from, to } ->
            List.indexedMap
                (\ind tok ->
                    if from <= ind && ind <= to then
                        f tok

                    else
                        [ tok ]
                )
                >> List.concat


addCursor : Position -> List Token -> List Token
addCursor cursor =
    let
        cursorTransformer =
            List.singleton
                >> Html.span
                    [ HA.style "background-color" "orange"
                    ]
    in
    -- Fix `mapRange (Empty cursor)` type of error
    mapRange (Inclusive { from = cursor, to = cursor })
        (\token ->
            case token of
                CharTok ts ch ->
                    [ CharTok (cursorTransformer :: ts) <| ch ]

                NewLine ->
                    [ CharTok [ cursorTransformer ] '\u{00A0}', NewLine ]
        )


lineLengths : Buffer -> List Int
lineLengths =
    (\x -> String.append x "\n")
        >> String.toList
        >> List.Extra.groupWhile (\tok _ -> tok /= '\n')
        >> List.map ((\( a, b ) -> a :: b) >> List.length)


toIndex : Buffer -> { line : Int, column : Int } -> Int
toIndex buff { line, column } =
    buff
        |> lineLengths
        |> List.take (line + 1)
        |> List.Extra.unconsLast
        |> Maybe.map (\( x, xs ) -> List.sum xs + min column (x - 1))
        |> Maybe.withDefault column


toVisPos : Buffer -> Int -> { line : Int, column : Int }
toVisPos buff pos =
    buff
        |> lineLengths
        |> List.Extra.scanl1 (+)
        |> List.Extra.takeWhile ((>=) pos)
        |> (\l -> { line = List.length l, column = List.Extra.last l |> Maybe.withDefault 0 |> (-) pos })


tokensToHtml : List Token -> List (List (Html Msg))
tokensToHtml =
    List.Extra.unconsLast -- remove NewLine on end
        >> Maybe.map Tuple.second
        >> Maybe.withDefault []
        >> List.Extra.indexedFoldl
            (\pos tok acc ->
                case ( tok, acc ) of
                    ( CharTok ts ch, x :: xs ) ->
                        (( pos, ts, ch ) :: x) :: xs

                    ( CharTok ts ch, [] ) ->
                        [ ( pos, ts, ch ) ] |> List.singleton

                    _ ->
                        [] :: acc
            )
            []
        >> List.map (List.map transform >> List.reverse)
        >> List.reverse


moveCursor : Direction -> Buffer -> Position -> Position
moveCursor dir buff cursor =
    case dir of
        Horizontal delta ->
            cursor + delta |> clamp 0 ((lineLengths buff |> List.sum) - 1)

        Vertical delta ->
            cursor
                |> toVisPos buff
                |> (\{ line, column } -> { line = line + delta, column = column })
                |> toIndex buff


type alias Transformer =
    Html Msg -> Html Msg


type Token
    = CharTok (List Transformer) Char
    | NewLine


transform : ( Position, List Transformer, Char ) -> Html Msg
transform ( pos, ts, ch ) =
    List.foldl (\t acc -> t acc)
        (Html.span
            [ Html.Events.custom "mouseover"
                (JD.succeed
                    { message = Hover (HoverChar pos)
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
            ]
         <|
            [ Html.text <| String.fromChar ch ]
        )
        ts


toTokens : Buffer -> List Token
toTokens buf =
    String.append buf "\n"
        |> String.toList
        |> List.map
            (\ch ->
                if ch == '\n' then
                    NewLine

                else
                    CharTok [] ch
            )
