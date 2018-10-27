module TypesTest exposing (suite)

import Basics exposing (Float(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Magic exposing (..)
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
        , describe "estimateIn"
            [ test "given a summonsNoQuestOneFellow returns a scope of 0 hours" <|
                \() ->
                    Expect.equal
                        (estimateIn Hours <| Summons emptyQuest (Fellowship [ { austin | history = shortHistory } ]))
                        (In Hours 0)
            , test "given a Summons with no quest and no fellowship returns a scope of 0 days" <|
                \() ->
                    Expect.equal
                        (estimateIn Days <| Summons emptyQuest (Fellowship [ { austin | history = shortHistory } ]))
                        (In Days 0)
            , test "given a Summons with no quest and no fellowship returns a scope of 0 weeks" <|
                \() ->
                    Expect.equal
                        (estimateIn Weeks <| Summons emptyQuest (Fellowship [ { austin | history = shortHistory } ]))
                        (In Weeks 0)
            , test "given a shortSummonsNoFellows returns Infinity" <|
                \() ->
                    Expect.equal (estimateIn Hours shortSummonsNoFellows) (In Hours <| round (1 / 0))
            , test "given a shortSummonsNoFellows returns Infinity in Days" <|
                \() ->
                    Expect.equal (estimateIn Days shortSummonsNoFellows) (In Days <| round (1 / 0))
            , test "given a shortSummonsNoFellows returns Infinity in Weeks" <|
                \() ->
                    Expect.equal (estimateIn Weeks shortSummonsNoFellows) (In Weeks <| round (1 / 0))
            , test "given a Summons with a empty Quest and an non-empty Fellowship returns 0" <|
                \() ->
                    -- Given a Quest of 0 points
                    -- I should expect the Hourly estimate to be 0 as there's no work to do.
                    Expect.equal (estimateIn Hours summonsNoQuestOneFellow) (In Hours 0)
            , test "given a fiftyPointSummons with austin returns in Hours" <|
                \() ->
                    Expect.equal
                        (estimateIn Hours (fiftyPointSummons (Fellowship [ austin ])))
                        (In Hours 267)
            , test "given a fiftyPointSummons with austin returns in Days" <|
                \() ->
                    Expect.equal
                        (estimateIn Days (fiftyPointSummons (Fellowship [ austin ])))
                        (In Days 33)
            , test "given a fiftyPointSummons with austin returns in Weeks" <|
                \() ->
                    Expect.equal
                        (estimateIn Weeks (fiftyPointSummons (Fellowship [ austin ])))
                        (In Weeks 7)
            , test "given a fiftyPointSummons returns In Hours" <|
                \() ->
                    Expect.equal (estimateIn Hours (fiftyPointSummons (Fellowship [ austin, dom ]))) (In Hours 138)
            , test "given a fiftyPointSummons returns In Days" <|
                \() ->
                    Expect.equal (estimateIn Days (fiftyPointSummons (Fellowship [ austin, dom ]))) (In Days 17)
            , test "given a fiftyPointSummons returns In Weeks" <|
                \() ->
                    Expect.equal (estimateIn Weeks (fiftyPointSummons (Fellowship [ austin, dom ]))) (In Weeks 3)
            ]
        , describe "castSpell"
            [ test "adds mastery from incantation to sourcerers prowess" <|
                \() ->
                    let
                        { prowess } =
                            castSpell
                                (fromCalendarDate 2018 Oct 23)
                                dom
                                (Incantation
                                    (AsA joe
                                        (IWantTo "change the color of the button to blue")
                                        (SoThat "it is consistent with the site theme.")
                                    )
                                    (GuessOf One (MasterOf editCode))
                                )
                    in
                    Expect.equal prowess (Prowess <| Magic.new [ oop, php, editCode ])
            , test "doesn't add duplicate skills from mastery to sourcerers prowess" <|
                \() ->
                    let
                        { prowess } =
                            castSpell
                                (fromCalendarDate 2018 Oct 23)
                                dom
                                (Incantation
                                    (AsA joe
                                        (IWantTo "change the color of the button to blue")
                                        (SoThat "it is consistent with the site theme.")
                                    )
                                    (GuessOf One (MasterOf php))
                                )
                    in
                    Expect.equal prowess (Prowess <| Magic.new [ oop, php ])
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
    , prowess =
        Prowess <|
            Magic.new
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
    , prowess =
        Prowess <|
            Magic.new
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


shortHistory : Tale
shortHistory =
    Past
        [ Deed One (fromCalendarDate 2018 Oct 22)
        ]


shortSummonsNoFellows : Summons
shortSummonsNoFellows =
    Summons
        (Quest
            [ Incantation
                (AsA joe
                    (IWantTo "update the color of the button")
                    (SoThat "it is consistent with the theme")
                )
                (GuessOf Two (MasterOf editCode))
            ]
        )
        emptyFellowship


summonsNoQuestOneFellow : Summons
summonsNoQuestOneFellow =
    Summons emptyQuest (Fellowship [ austin ])


fiftyPointSummons : Fellowship -> Summons
fiftyPointSummons =
    Summons
        (Quest
            [ Incantation
                (AsA joe
                    (IWantTo "build an index page for Users")
                    (SoThat "I can see who's using the system")
                )
                (GuessOf Eight (MasterOf git))
            , Incantation
                (AsA joe
                    (IWantTo "update the color of the button")
                    (SoThat "it is consistent with the theme")
                )
                (GuessOf Two (MasterOf <| Magic.new [ editCode, git ]))
            , Incantation
                (AsA joe
                    (IWantTo "implement search")
                    (SoThat "I can find what I'm looking for.")
                )
                (GuessOf Thirteen (MasterOf <| Magic.new [ editCode, git ]))
            , Incantation
                (AsA joe
                    (IWantTo "deploy my app")
                    (SoThat "I can see it somewhere")
                )
                (GuessOf Thirteen (MasterOf <| Magic.new [ editCode, git ]))
            , Incantation
                (AsA joe
                    (IWantTo "Integrate with 3rd Party Oauth client")
                    (SoThat "users can connect their profiles")
                )
                (GuessOf Thirteen (MasterOf <| Magic.new [ editCode, git ]))
            , Incantation
                (AsA joe
                    (IWantTo "change the color of the button to blue")
                    (SoThat "it is consistent with the site theme.")
                )
                (GuessOf One (MasterOf editCode))
            ]
        )


fifteenPointsPerSprintHistory : Tale
fifteenPointsPerSprintHistory =
    Past
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


fourteenPointAvgPointsPerSprint : Tale
fourteenPointAvgPointsPerSprint =
    Past
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
