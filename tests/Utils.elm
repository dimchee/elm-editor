module Utils exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
import Test exposing (..)


suite : Test
suite =
    describe "Simple utility functions test"
        [ test "getMidle" <|
            \_ ->
                Expect.equal ( Just "te", "sti", Just "ng" )
                    <| Main.getMiddle (Main.Inclusive { from = 2, to = 4 }) "testing"
        , test "getMidle2" <|
            \_ ->
                Expect.equal ( Nothing, "te", Just "sting" )
                    <| Main.getMiddle (Main.Inclusive { from = 0, to = 1 }) "testing"
        , test "getMidle3" <|
            \_ ->
                Expect.equal ( Just "test", "ing", Nothing )
                    <| Main.getMiddle (Main.Inclusive { from = 4, to = 6 }) "testing"
        , test "getMidle4" <|
            \_ ->
                Expect.equal ( Nothing, "", Just "testing" )
                    <| Main.getMiddle (Main.Empty 0) "testing"
        , test "getMidle5" <|
            \_ ->
                Expect.equal ( Just "testing", "", Nothing )
                    <| Main.getMiddle (Main.Inclusive { from = 7, to = 6 }) "testing"
        , test "getMidle6" <|
            \_ ->
                Expect.equal ( Nothing, "", Just "t" )
                    <| Main.getMiddle (Main.Empty 0) "t"
        , test "getMidle7" <|
            \_ ->
                Expect.equal ( Just "tes", "", Just "ting" )
                    <| Main.getMiddle (Main.Empty 3) "testing"
        , test "mapRange" <|
            \_ ->
                Expect.equal [ Main.Str [] "a", Main.NewLine ] <|
                    Main.mapRange (Main.Empty 0)
                        (\_ -> Main.Str [] "a")
                    <|
                        Main.toTokens [ "" ]
        ]
