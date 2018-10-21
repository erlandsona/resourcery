module TypesTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    -- Use Case / User Story / Business Goal
    describe "who's available to work on what when?"
        -- [Function|Set of functions] that answer this question
        [ describe "estimate"
            [ test "given a Summons with no quest and no prowess returns the scope of a project to be 0 hours" <|
                \() ->
                    Expect.equal
                        (estimate <| Summons (Quest []))
                        (Fellowship [ Sourcerer {} ])
                        (Hours 0)
            , test "given a Summons with a non-empty Quest and empty Prowess" <|
                \() ->
                    let
                        editCode =
                            Skill "edit code"

                        git =
                            Skill "git"

                        summons =
                            Summons
                                (Quest
                                    [ Incantation
                                        (AsA
                                            (Patron (Name "Joe"))
                                            (IWantTo "change the text")
                                            (SoThat "it is blue.")
                                        )
                                        (GuessOf One (Mastery [ editCode ]))
                                    , Incantation
                                        (AsA (Patron (Name "Joe"))
                                            (IWantTo "update the color of the button")
                                            (SoThat "it is consistent with the theme")
                                        )
                                        (GuessOf Two (Mastery [ editCode, git ]))
                                    ]
                                )
                                Fellowship
                                [ Sourcerer {} ]
                    in
                    Expect.equal
                        (estimate GuessOf summons)
                        (Hours 3)
            ]
        , describe "velocity"
            [ test "given a Summons returns the scope of a project relative to the Pay Period" <|
                \() ->
                    Expect.equal
                        2
                        (1 + 1)
            ]
        ]
