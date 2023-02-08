module Main exposing (..)

import Browser
import Browser.Dom
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Json.Decode as JD
import Task


type alias Cursor =
    Int



-- TODO buffer could be array


type alias Model =
    { buffer : List Token
    , cursor : Cursor
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
    { buffer = toTokens [ "line1", "line2", "line3 aaa" ]
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


viewDebug : { a | buffer : List Token, cursor : Cursor } -> Html msg
viewDebug { buffer, cursor } =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ Html.div [] [ Html.text <| Debug.toString <| cursorToPos buffer cursor ]
        , Html.div []
            [ Html.text <| Debug.toString <| posToCursor buffer <| cursorToPos buffer cursor
            ]

        -- , Html.div [] [ Html.text <| Debug.toString <| buffer ]
        , Html.div [] [ Html.text <| Debug.toString <| tokensToRanges buffer ]

        -- , Html.div [] [ Html.text <| Debug.toString <| indexedMap (\{range, token} -> [ (range, token) ] ) buffer ]
        ]


addCursor : Cursor -> List Token -> List Token
addCursor cursor =
    let
        cursorTransformer =
            List.singleton
                >> Html.span
                    [ HA.style "background-color" "orange"
                    ]
    in
    indexedMap
        (\{ range, token } ->
            let
                pos =
                    cursor - range.from
            in
            if range.from <= cursor && cursor <= range.to then
                case token of
                    Str ts str ->
                        [ Str ts <| String.dropRight (range.to - cursor + 1) str
                        , Str (cursorTransformer :: ts) <| String.slice pos (pos + 1) str
                        , Str ts <| String.dropLeft (cursor - range.from + 1) str
                        ]

                    _ ->
                        [ Str [ cursorTransformer ] "\u{00A0}", token ]

            else
                [ token ]
        )


tokensToRanges : List Token -> List Range
tokensToRanges tokens =
    tokens
        |> indexedMap
            (\{ range, token } ->
                Just range
                    :: (if token /= NewLine then
                            []

                        else
                            [ Nothing ]
                       )
            )
        |> List.foldl
            (\tokRange ranges ->
                case ( ranges, tokRange ) of
                    ( (Just r1) :: rs, Just r2 ) ->
                        (Just <| rangeJoin r1 r2) :: rs

                    ( Nothing :: rs, _ ) ->
                        tokRange :: rs

                    _ ->
                        tokRange :: ranges
            )
            []
        |> List.filterMap identity
        |> List.reverse


tokensToHtml : List Token -> List (List (Html Msg))
tokensToHtml =
    List.foldr
        (\tok lines ->
            case ( tok, lines ) of
                ( Str ts str, line :: rest ) ->
                    (transform ts str :: line) :: rest

                ( Str ts str, [] ) ->
                    [ [ transform ts str ] ]

                ( NewLine, _ ) ->
                    [] :: lines

                ( Blank, _ ) ->
                    lines
        )
        []


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
            |> List.filter (\x -> x == NewLine || x == Blank)
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
            -- |> indexedMap (\_ -> Maybe.map (String.fromChar >> Html.text))
            -- |> addCursor cursor
            -- |> viewText
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


moveCursor : Direction -> List Token -> Cursor -> Cursor
moveCursor dir tokens cursor =
    clamp 0 ((List.map length tokens |> List.sum) - 1) <|
        case dir of
            Horizontal delta ->
                cursor + delta

            Vertical delta ->
                cursor
                    |> cursorToPos tokens
                    |> (\{ line, column } -> { line = line + delta, column = column })
                    |> posToCursor tokens


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move dir ->
            ( { model | cursor = moveCursor dir model.buffer model.cursor }, Cmd.none )

        -- Insert char -> ( addChar char model, Cmd.none )
        _ ->
            ( model, Cmd.none )


type alias Transformer =
    Html Msg -> Html Msg


type Token
    = Str (List Transformer) String
    | NewLine
    | Blank


transform : List Transformer -> String -> Html Msg
transform ts str =
    List.foldl (\t acc -> t acc) (Html.text str) ts


length : Token -> Int
length tok =
    case tok of
        Str _ str ->
            String.length str

        NewLine ->
            1

        Blank ->
            1


toTokens : List String -> List Token
toTokens =
    List.map (Str []) >> List.intersperse NewLine >> (\x -> x ++ [ Blank ])


type alias Position =
    { line : Int
    , column : Int
    }


type alias Range =
    { from : Int
    , to : Int
    }


type alias IndexedToken =
    { range : Range
    , token : Token
    }


indexedMap : (IndexedToken -> List a) -> List Token -> List a
indexedMap f =
    let
        go tok ( indToks, acc ) =
            let
                len =
                    length tok
            in
            ( { range = { from = acc, to = acc + len - 1 }, token = tok } :: indToks, acc + len )
    in
    List.foldl go ( [], 0 )
        >> Tuple.first
        >> List.concatMap (\indTok -> f indTok |> List.reverse)
        >> List.reverse


rangeJoin : Range -> Range -> Range
rangeJoin { from } { to } =
    { from = from, to = to }



-- TODO Should make 2 way map (from pos to cursor and vice verca)


cursorToPos : List Token -> Cursor -> Position
cursorToPos tokens cursor =
    let
        clamped =
            max cursor 0
    in
    tokens
        |> List.foldl
            (\tok ( sol, ( pos, ln, col ) ) ->
                let
                    (( newPos, _, _ ) as new) =
                        if tok == NewLine then
                            ( pos + length tok, ln + 1, 0 )

                        else
                            ( pos + length tok, ln, col + length tok )
                in
                if pos <= clamped && clamped <= newPos then
                    ( Just { line = ln, column = col + clamped - pos }, new )

                else
                    ( sol, new )
            )
            ( Nothing, ( 0, 0, 0 ) )
        |> (\( mpos, ( _, ln, col ) ) -> Maybe.withDefault { line = ln, column = col } mpos)


posToCursor : List Token -> Position -> Cursor
posToCursor tokens { line, column } =
    let
        clampedLine = clamp 0 (List.filter ((==) NewLine) tokens |> List.length) line
    in
    tokens
        |> List.foldl
            (\tok ( sol, ( pos, ln, col ) ) ->
                let
                    (( _, _, newCol ) as new) =
                        if tok == NewLine then
                            ( pos + length tok, ln + 1, 0 )

                        else
                            ( pos + length tok, ln, col + length tok )
                in
                -- token can't be on more than one line
                if ln == clampedLine && tok == NewLine && col < column then
                    ( Just <| pos, new )

                else if ln == clampedLine && col <= column && column <= newCol then
                    ( Just <| pos + column - col, new )

                else
                    ( sol, new )
            )
            ( Nothing, ( 0, 0, 0 ) )
        |> (\( mcursor, (cursor, _, _)) -> Maybe.withDefault cursor mcursor)
