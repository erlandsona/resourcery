module TypesTest exposing (suite)

import Basics exposing (Float(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time exposing (Month(..), utc)
import Time.Extra as TimE exposing (Parts)
import Types exposing (..)


suite : Test
suite =
    -- Use Case / User Story / Business Goal
    describe "who's available to work on what when?"
        -- [Function|Set of functions] that answer this question
        [ describe "velocity"
            [ test "Given a Sourcerer it returns their velocity as a weighted avg of the Deeds they've done." <|
                \() ->
                    Expect.equal (velocity { austin | history = fourteenPointAvgPointsPerSprint }) 14
            , test "Given a Sourcerer with couple tales@15 points per" <|
                \() ->
                    Expect.equal (velocity austin) 15
            , test "Given a Sourcerer with a single tale@1 point" <|
                \() -> Expect.equal (velocity { austin | history = shortHistory }) 1
            ]
        , describe "estimate"
            [ test "given a Summons with no quest and no fellowship returns a scope of 0 hours" <|
                \() ->
                    Expect.equal
                        (estimate <| Summons emptyQuest (Fellowship [ { austin | history = shortHistory } ]))
                        (Hours 0)
            , test "given a Summons with a non-empty Quest and an empty Fellowship returns Infinity" <|
                \() ->
                    let
                        summons =
                            Summons
                                (Quest
                                    [ Incantation
                                        (AsA joe
                                            (IWantTo "update the color of the button")
                                            (SoThat "it is consistent with the theme")
                                        )
                                        (GuessOf Two (MasterOf [ editCode, git ]))
                                    ]
                                )
                                emptyFellowship
                    in
                    Expect.equal (estimate summons) (Hours <| round (1 / 0))
            , test "given a Summons with a empty Quest and an non-empty Fellowship returns 0" <|
                \() ->
                    let
                        summons =
                            Summons emptyQuest (Fellowship [ austin ])
                    in
                    -- Given a Quest of 0 points
                    -- I should expect the Hourly estimate to be 0 as there's no work to do.
                    Expect.equal (estimate summons) (Hours 0)
            , test "given a Summons with a non-empty Quest and a single Sourcer" <|
                \() ->
                    let
                        summons =
                            Summons
                                (Quest
                                    -- requesting 10 points
                                    [ Incantation
                                        (AsA joe
                                            (IWantTo "build an index page for Users")
                                            (SoThat "I can see who's using the system")
                                        )
                                        (GuessOf Eight (MasterOf [ git ]))
                                    , Incantation
                                        (AsA joe
                                            (IWantTo "update the color of the button")
                                            (SoThat "it is consistent with the theme")
                                        )
                                        (GuessOf Two (MasterOf [ editCode, git ]))
                                    , Incantation
                                        (AsA joe
                                            (IWantTo "implement search")
                                            (SoThat "I can find what I'm looking for.")
                                        )
                                        (GuessOf Thirteen (MasterOf [ editCode, git ]))
                                    , Incantation
                                        (AsA joe
                                            (IWantTo "deploy my app")
                                            (SoThat "I can see it somewhere")
                                        )
                                        (GuessOf Thirteen (MasterOf [ editCode, git ]))
                                    , Incantation
                                        (AsA joe
                                            (IWantTo "Integrate with 3rd Party Oauth client")
                                            (SoThat "users can connect their profiles")
                                        )
                                        (GuessOf Thirteen (MasterOf [ editCode, git ]))
                                    , Incantation
                                        (AsA joe
                                            (IWantTo "change the color of the button to blue")
                                            (SoThat "it is consistent with the site theme.")
                                        )
                                        (GuessOf One (MasterOf [ editCode ]))
                                    ]
                                )
                                (Fellowship [ austin ])
                    in
                    -- Given a Quest of 50 points
                    -- And a velocity averaging 15 points per sprint
                    -- I should expect the Hourly estimate to be ~ 267 hours
                    Expect.equal
                        (estimate summons)
                        -- because 50 / 15 * 80hours per sprint ~ 267
                        (Hours 267)
            , test "given a Summons with a non-empty Quest and a couple in the fellowship" <|
                \() ->
                    let
                        summons =
                            Summons
                                (Quest
                                    -- requesting 10 points
                                    [ Incantation
                                        (AsA joe
                                            (IWantTo "build an index page for Users")
                                            (SoThat "I can see who's using the system")
                                        )
                                        (GuessOf Eight (MasterOf [ git ]))
                                    , Incantation
                                        (AsA joe
                                            (IWantTo "update the color of the button")
                                            (SoThat "it is consistent with the theme")
                                        )
                                        (GuessOf Two (MasterOf [ editCode, git ]))
                                    , Incantation
                                        (AsA joe
                                            (IWantTo "implement search")
                                            (SoThat "I can find what I'm looking for.")
                                        )
                                        (GuessOf Thirteen (MasterOf [ editCode, git ]))
                                    , Incantation
                                        (AsA joe
                                            (IWantTo "deploy my app")
                                            (SoThat "I can see it somewhere")
                                        )
                                        (GuessOf Thirteen (MasterOf [ editCode, git ]))
                                    , Incantation
                                        (AsA joe
                                            (IWantTo "Integrate with 3rd Party Oauth client")
                                            (SoThat "users can connect their profiles")
                                        )
                                        (GuessOf Thirteen (MasterOf [ editCode, git ]))
                                    , Incantation
                                        (AsA joe
                                            (IWantTo "change the color of the button to blue")
                                            (SoThat "it is consistent with the site theme.")
                                        )
                                        (GuessOf One (MasterOf [ editCode ]))
                                    ]
                                )
                                (Fellowship [ austin, dom ])
                    in
                    -- Given a Quest of 50 points
                    -- And a velocity averaging 15 points per sprint
                    -- I should expect the Hourly estimate to be ~ 138 hours
                    Expect.equal
                        (estimate summons)
                        -- because 50 / 29 * 80hours per sprint ~ 138
                        (Hours 138)
            ]
        ]


joe : Account
joe =
    Patron (Named "Joe")


dom : Sourcerer
dom =
    { casting = Nothing
    , named = Named "Dominic Serrano"
    , future = Pipeline []
    , history = fourteenPointAvgPointsPerSprint
    , skills =
        Prowess
            [ oop
            , php
            ]
    }


austin : Sourcerer
austin =
    -- , summons = currentSummons
    { casting = Nothing
    , named = Named "Austin Erlandson"
    , future = Pipeline [ Summons (Quest []) (Fellowship []) ]
    , history = fifteenPointsPerSprintHistory
    , skills =
        Prowess
            [ ruby
            , php
            , oop
            , devOps
            ]
    }


emptyFellowship : Fellowship
emptyFellowship =
    Fellowship []


emptyQuest : Quest
emptyQuest =
    Quest []


shortHistory : History
shortHistory =
    Historical
        [ Deed One (fromCalendarDate 2018 Oct 22)
        ]


fifteenPointsPerSprintHistory : History
fifteenPointsPerSprintHistory =
    Historical
        -- Should avg to 15 points
        -- 15 Points
        [ Deed Five (fromCalendarDate 2018 Oct 23)
        , Deed One (fromCalendarDate 2018 Oct 22)
        , Deed Three (fromCalendarDate 2018 Oct 21)
        , Deed One (fromCalendarDate 2018 Oct 20)

        -- 15 Points
        , Deed Five (fromCalendarDate 2018 Oct 19)
        , Deed One (fromCalendarDate 2017 Oct 21)
        , Deed Three (fromCalendarDate 2017 Oct 21)
        , Deed Eight (fromCalendarDate 2017 Oct 21)
        , Deed Three (fromCalendarDate 2017 Oct 21)
        ]


fourteenPointAvgPointsPerSprint : History
fourteenPointAvgPointsPerSprint =
    Historical
        -- Should avg to 15 points
        [ Deed One (fromCalendarDate 2018 Jan 1)

        -- 15 Points
        , Deed Five (fromCalendarDate 2018 Oct 23)
        , Deed One (fromCalendarDate 2018 Oct 22)
        , Deed Three (fromCalendarDate 2018 Oct 21)
        , Deed One (fromCalendarDate 2018 Oct 20)
        , Deed Five (fromCalendarDate 2018 Oct 19)

        -- 15 Points
        , Deed One (fromCalendarDate 2017 Oct 21)
        , Deed Three (fromCalendarDate 2017 Oct 21)
        , Deed Eight (fromCalendarDate 2017 Oct 21)
        , Deed Three (fromCalendarDate 2017 Oct 21)
        ]


fromCalendarDate y m d =
    Parts y m d 0 0 0 0
        |> TimE.partsToPosix utc


editCode : Skill
editCode =
    Skill "edit code"


git : Skill
git =
    Skill "git"
