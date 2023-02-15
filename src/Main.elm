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


type Range
    = Empty Position
    | Inclusive
        { from : Position
        , to : Position
        }


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
    | Insert Token
    | Delete


init : Model
init =
    { buffer = toTokens [ "line1", "line2", "line3 aaa", "line4" ]
    , cursor = 2
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


clean : List Token -> List Token
clean =
    List.foldr
        (\tok acc ->
            case ( tok, acc ) of
                ( Str [] str1, (Str [] str2) :: xs ) ->
                    Str [] (str1 ++ str2) :: xs

                _ ->
                    tok :: acc
        )
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ buffer, cursor } as model) =
    case msg of
        Move dir ->
            ( { model | cursor = moveCursor dir buffer cursor }, Cmd.none )

        Insert tok ->
            ( { buffer =
                    clean <|
                        mapRange (Empty cursor)
                            (\_ -> [ tok ])
                            buffer
              , cursor = cursor + 1
              }
            , Cmd.none
            )

        Delete ->
            ( if cursor > 0 then
                { buffer =
                    clean <|
                        mapRange (Inclusive { from = cursor - 1, to = cursor - 1 })
                            (\_ -> [])
                            buffer
                , cursor = cursor - 1
                }

              else
                model
            , Cmd.none
            )

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
        , Html.div [] [ Html.text <| Debug.toString <| buffer ]
        , Html.div [] [ Html.text <| Debug.toString <| tokensToRanges buffer ]
        , Html.div [] [ Html.text <| Debug.toString <| toLines buffer ]
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
            |> List.map (Html.div [ HA.style "height" "24px" ])
            |> Html.div
                [ HA.style "background-color" "#f0f0f0"
                , HA.style "width" "100%"
                ]
        ]


keyToMsg : String -> JD.Decoder Msg
keyToMsg string =
    case string of
        "ArrowUp" ->
            JD.succeed <| Move <| Vertical -1

        "ArrowDown" ->
            JD.succeed <| Move <| Vertical 1

        "ArrowRight" ->
            JD.succeed <| Move <| Horizontal 1

        "ArrowLeft" ->
            JD.succeed <| Move <| Horizontal -1

        "Tab" ->
            JD.succeed <| Insert <| Str [] "\t"

        "Enter" ->
            JD.succeed <| Insert NewLine

        "Backspace" ->
            JD.succeed <| Delete

        _ ->
            case string |> String.toList of
                [ ' ' ] ->
                    JD.succeed <| Insert <| Str [] "\u{00A0}"

                [ char ] ->
                    JD.succeed <| Insert <| Str [] <| String.fromChar char

                _ ->
                    JD.fail "Key not bound"


tokensToRanges : List Token -> List Range
tokensToRanges =
    List.map (\tok -> ( 0, length tok - 1 ))
        >> List.Extra.scanl1 (\( _, len ) ( _, end ) -> ( end + 1, len + 1 |> (+) end ))
        >> List.map
            (\( start, end ) ->
                when (start <= end) (Inclusive { from = start, to = end })
                    |> Maybe.withDefault (Empty start)
            )


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


when : Bool -> a -> Maybe a
when test value =
    if test then
        Just value

    else
        Nothing


localTo : Range -> Range -> Maybe Range
localTo frame range =
    case ( frame, range ) of
        ( Inclusive { from, to }, Inclusive r ) ->
            Just r
                |> Maybe.Extra.filter (\x -> x.from <= x.to)
                |> Maybe.Extra.filter (\x -> from <= x.to && x.from <= to)
                |> Maybe.map (\x -> { from = x.from - from, to = x.to - from })
                |> Maybe.map
                    (\targ ->
                        { from = clamp 0 (to - from) <| targ.from
                        , to = clamp 0 (to - from) <| targ.to
                        }
                    )
                |> Maybe.map Inclusive

        ( Empty pos, Inclusive { from, to } ) ->
            when (from <= pos && pos <= to) <| Empty 0

        ( Inclusive { from, to }, Empty pos ) ->
            when (from <= pos && pos <= to) <| Empty <| pos - from

        ( Empty pos1, Empty pos2 ) ->
            when (pos1 == pos2) <| Empty 0


mapRange : Range -> (Token -> List Token) -> List Token -> List Token
mapRange target f tokens =
    List.map2 (\tok range -> ( tok, localTo range target )) tokens (tokensToRanges tokens)
        |> List.concatMap
            (\ranged ->
                case ranged of
                    ( Str ts str, Just localRange ) ->
                        getMiddle localRange str
                            |> (\( left, mid, right ) ->
                                    (List.map (Str ts) <| Maybe.Extra.toList left)
                                        ++ (f <| Str ts <| mid)
                                        ++ (List.map (Str ts) <| Maybe.Extra.toList right)
                               )

                    ( NewLine, Just localRange ) ->
                        case localRange of
                            Empty _ ->
                                (f <| NewLine) ++ [ NewLine ]

                            Inclusive _ ->
                                f <| NewLine

                    ( tok, Nothing ) ->
                        [ tok ]
            )


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
                Str ts str ->
                    [ Str (cursorTransformer :: ts) <| str ]

                NewLine ->
                    [ Str [ cursorTransformer ] "\u{00A0}", NewLine ]
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
        >> List.map
            (\nonEmpty ->
                case nonEmpty of
                    ( NewLine, _ ) ->
                        [ Html.text "" ]

                    ( Str ts x, xs ) ->
                        transform ( ts, x ) :: (xs |> List.filterMap toMaybe |> List.map transform)
            )


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
    List.filter (not << String.isEmpty)
        >> List.map (Str [])
        >> List.intersperse NewLine
        >> (\x -> x ++ [ NewLine ])
