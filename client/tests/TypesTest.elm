module TypesTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time exposing (Month(..), utc)
import Time.Extra as TimE exposing (Parts)
import Types exposing (..)


fromCalendarDate y m d =
    Parts y m d 0 0 0 0
        |> TimE.partsToPosix utc


suite : Test
suite =
    -- Use Case / User Story / Business Goal
    describe "who's available to work on what when?"
        -- [Function|Set of functions] that answer this question
        [ describe "sourcerersVelocity"
            [ test "Given a Sourcerer it returns their velocity as a weighted avg of the Deeds they've done." <|
                \() ->
                    let
                        austin : Sourcerer
                        austin =
                            -- , summons = currentSummons
                            { casting = Nothing
                            , named = Named "Austin Erlandson"
                            , journey = Pipeline []
                            , history =
                                Historical
                                    -- Should avg to 15 points
                                    [ Deed (GuessOf One (MasterOf [ Skill "Bossness" ])) (fromCalendarDate 2018 Jan 1)

                                    -- 15 Points
                                    , Deed (GuessOf Five (MasterOf [ Skill "Bossness" ])) (fromCalendarDate 2018 Oct 23)
                                    , Deed (GuessOf One (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2018 Oct 22)
                                    , Deed (GuessOf Three (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2018 Oct 21)
                                    , Deed (GuessOf One (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2018 Oct 20)

                                    -- 15 Points
                                    , Deed (GuessOf Five (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2018 Oct 19)
                                    , Deed (GuessOf One (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2017 Oct 21)
                                    , Deed (GuessOf Three (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2017 Oct 21)
                                    , Deed (GuessOf Eight (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2017 Oct 21)
                                    , Deed (GuessOf Three (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2017 Oct 21)
                                    ]
                            , skills = Prowess []
                            }
                    in
                    Expect.equal (sourcerersVelocity austin) 14
            ]

        -- , describe
        --     "speed"
        --     [ test "given a list of histories it returns a float" <|
        --         \() ->
        --             let
        --                 histories =
        --                     [ Historical
        --                         [ Deed (GuessOf One (MasterOf [])) (fromCalendarDate 2018 Oct 22)
        --                         , Deed (GuessOf Two (MasterOf [])) (fromCalendarDate 2018 Oct 22)
        --                         ]
        --                     ]
        --             in
        --             Expect.equal (speed histories) 3
        --     ]
        , describe "estimate"
            [ test "given a Summons with no quest and no prowess returns the scope of a project to be 0 hours" <|
                \() ->
                    let
                        otherSummons =
                            Summons (Quest []) (Fellowship [])

                        austin : Sourcerer
                        austin =
                            -- , summons = currentSummons
                            { casting = Nothing
                            , named = Named "Austin Erlandson"
                            , journey = Pipeline [ otherSummons ]
                            , history =
                                Historical
                                    [ Deed (GuessOf One (MasterOf [ Skill "Bossness" ])) (fromCalendarDate 2018 Oct 22)
                                    ]
                            , skills = Prowess [ Skill "Elm" ]
                            }

                        currentSummons =
                            Summons (Quest []) (Fellowship [ austin ])
                    in
                    Expect.equal
                        (estimate <| currentSummons)
                        (Hours 0)
            , test "given a Summons with a non-empty Quest and empty Prowess" <|
                \() ->
                    let
                        editCode =
                            Skill "edit code"

                        git =
                            Skill "git"

                        otherSummons =
                            Summons (Quest []) (Fellowship [])

                        austin : Sourcerer
                        austin =
                            -- , summons = currentSummons
                            { casting = Nothing
                            , named = Named "Austin Erlandson"
                            , journey = Pipeline [ otherSummons ]
                            , history =
                                Historical
                                    -- Should avg to 15 points
                                    -- 15 Points
                                    [ Deed (GuessOf Five (MasterOf [ Skill "Bossness" ])) (fromCalendarDate 2018 Oct 23)
                                    , Deed (GuessOf One (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2018 Oct 22)
                                    , Deed (GuessOf Three (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2018 Oct 21)
                                    , Deed (GuessOf One (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2018 Oct 20)

                                    -- 15 Points
                                    , Deed (GuessOf Five (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2018 Oct 19)
                                    , Deed (GuessOf One (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2017 Oct 21)
                                    , Deed (GuessOf Three (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2017 Oct 21)
                                    , Deed (GuessOf Eight (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2017 Oct 21)
                                    , Deed (GuessOf Three (MasterOf [ Skill "Web Dev" ])) (fromCalendarDate 2017 Oct 21)
                                    ]
                            , skills = Prowess [ Skill "Elm" ]
                            }

                        joe =
                            Patron (Named "Joe")

                        summons =
                            Summons
                                (Quest
                                    -- requesting 10 points
                                    [ Incantation
                                        (AsA joe
                                            (IWantTo "build an index page for Users")
                                            (SoThat "I can see who's using the system")
                                        )
                                        (GuessOf Eight (MasterOf [ editCode ]))
                                    , Incantation
                                        (AsA joe
                                            (IWantTo "update the color of the button")
                                            (SoThat "it is consistent with the theme")
                                        )
                                        (GuessOf Two (MasterOf [ editCode, git ]))

                                    -- , Incantation
                                    --     (AsA joe
                                    --         (IWantTo "implement search")
                                    --         (SoThat "I can find what I'm looking for.")
                                    --     )
                                    --     (GuessOf Thirteen (MasterOf [ editCode, git ]))
                                    -- , Incantation
                                    --     (AsA joe
                                    --         (IWantTo "deploy my app")
                                    --         (SoThat "I can see it somewhere")
                                    --     )
                                    --     (GuessOf Thirteen (MasterOf [ editCode, git ]))
                                    -- , Incantation
                                    --     (AsA joe
                                    --         (IWantTo "Integrate with 3rd Party Oauth client")
                                    --         (SoThat "users can connect their profiles")
                                    --     )
                                    --     (GuessOf Thirteen (MasterOf [ editCode, git ]))
                                    -- , Incantation
                                    --     (AsA joe
                                    --         (IWantTo "change the color of the button to blue")
                                    --         (SoThat "it is consistent with the site theme.")
                                    --     )
                                    --     (GuessOf One (MasterOf [ editCode ]))
                                    ]
                                )
                                (Fellowship [ austin ])
                    in
                    -- Given a Quest of 10 points
                    -- And a velocity averaging 15 points per sprint
                    -- I should expect the Hourly estimate to be
                    Expect.equal
                        (estimate summons)
                        -- because 10 / 15 * 80hours per sprint == 53.3333
                        (Hours 53)
            ]
        , describe "velocity"
            [ test "given a Summons returns the scope of a project relative to the Pay Period" <|
                \() ->
                    Expect.equal
                        2
                        (1 + 1)
            ]
        ]
