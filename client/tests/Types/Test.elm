module Types.Test exposing (suite)

import Account exposing (Person(..))
import Basics exposing (Float(..))
import Dict exposing (Dict)
import Estimate exposing (Scope(..))
import Expect exposing (Expectation)
import Fellow
import Fuzz exposing (Fuzzer)
import Helpers.Date exposing (cal)
import History exposing (Past)
import ID
import Incantation
    exposing
        ( Description(..)
        , Idea(..)
        , Philosophy(..)
        , Quest
        , Reason(..)
        )
import Journey exposing (Caesura(..))
import Magic exposing (Mastery(..), Prowess(..))
import Name exposing (..)
import Patron
import Prediction exposing (..)
import Result.Extra as ResultE
import Set exposing (Set)
import Sourcerer
import Spell
import Summons
import Test exposing (..)
import Time exposing (Month(..))
import Time.Extra exposing (Parts)
import World exposing (World)


suite : Test
suite =
    describe "Resourcery"
        {-

           Use Case / User Story / Business Goal

        -}
        [ describe "who's available to work on what when?"
            {-

               [Function|Set of functions] that answer this question

            -}
            [ describe "velocity"
                [ test "given fourteenPointsPerJourned returns a weighted avg of a Past" <|
                    \() ->
                        Expect.equal
                            14
                            (Sourcerer.velocity fourteenPointsPerJourney)
                , test "Given a tale with a couple Journeys@15 points per" <|
                    \() ->
                        Expect.equal
                            15
                            (Sourcerer.velocity fifteenPointsPerJourney)
                , test "Given a tale with a single Journey@1 point" <|
                    \() ->
                        Expect.equal 1 (Sourcerer.velocity shortPast)
                ]
            , describe "Estimate.in_"
                [ fuzz caesuraF "given summons not found returns Scope of 0" <|
                    \caesura ->
                        Expect.equal
                            (Approximately 0 caesura)
                            (Estimate.in_ caesura "blah" middleEarth)
                , fuzz caesuraF "given a emptySummons (Dict.empty has no keys) returns Scope of 0" <|
                    \caesura ->
                        Expect.equal
                            (Approximately 0 caesura)
                            (Estimate.in_ caesura "blah" { middleEarth | summons = emptySummons })
                , fuzz caesuraF "given a summonsNoQuestOneFellow returns Scope of 0" <|
                    \caesura ->
                        -- Given a Quest of 0 points
                        -- I should expect the Hourly estimate to be 0 as there's no work to do.
                        Expect.equal
                            (Approximately 0 caesura)
                            (Estimate.in_ caesura summonsNoQuestOneFellowId { middleEarth | summons = summonsNoQuestOneFellow })
                , fuzz caesuraF "given a shortSummonsNoFellows returns Infinity" <|
                    \caesura ->
                        Expect.equal
                            (Approximately (round (1 / 0)) caesura)
                            (Estimate.in_ caesura shortSummonsNoFellowsId { middleEarth | summons = shortSummonsNoFellows })
                , test "given a fiftyPointSummons with austin returns in Hours" <|
                    \() ->
                        Expect.equal
                            (Approximately 267 Hours)
                            (Estimate.in_ Hours fiftyPointSummonsId { middleEarth | summons = fiftyPointSummons })
                , test "given a fiftyPointSummons with austin returns in Days" <|
                    \() ->
                        Expect.equal
                            (Estimate.in_ Days fiftyPointSummonsId { middleEarth | summons = fiftyPointSummons })
                            (Approximately 33 Days)
                , test "given a fiftyPointSummons with austin returns in Weeks" <|
                    \() ->
                        Expect.equal
                            (Estimate.in_ Weeks fiftyPointSummonsId { middleEarth | summons = fiftyPointSummons })
                            (Approximately 7 Weeks)
                , test "given a fiftyPointSummons returns In Hours" <|
                    \() ->
                        Expect.equal
                            (Approximately 138 Hours)
                            (Estimate.in_ Hours fiftyPointSummonsTwoFellowsId { middleEarth | summons = fiftyPointSummonsTwoFellows })
                , test "given a fiftyPointSummons returns In Days" <|
                    \() ->
                        Expect.equal
                            (Approximately 17 Days)
                            (Estimate.in_ Days fiftyPointSummonsTwoFellowsId { middleEarth | summons = fiftyPointSummonsTwoFellows })
                , test "given a fiftyPointSummons returns In Weeks" <|
                    \() ->
                        Expect.equal
                            (Approximately 3 Weeks)
                            (Estimate.in_ Weeks fiftyPointSummonsTwoFellowsId { middleEarth | summons = fiftyPointSummonsTwoFellows })
                ]
            , describe "Spell.cast"
                [ test "adds mastery from incantation to sourcerers prowess" <|
                    \() ->
                        let
                            { skills } =
                                Spell.cast
                                    (cal 2018 Oct 23)
                                    domInfo
                                    incantation1Info
                        in
                        Expect.equal skills (Skill <| Set.fromList <| List.map Tuple.first [ ruby, php, editCode, git ])
                , test "doesn't add duplicate skills from mastery to sourcerers prowess" <|
                    \() ->
                        let
                            { skills } =
                                Spell.cast
                                    (cal 2018 Oct 23)
                                    domInfo
                                    incantation2Info
                        in
                        Expect.equal skills (Skill <| Set.fromList <| List.map Tuple.first [ ruby, php ])
                ]
            ]
        ]



{-
   Data
-}


caesuraF : Fuzzer Caesura
caesuraF =
    Fuzz.oneOf <| List.map Fuzz.constant [ Hours, Days, Weeks ]



-- descriptionF : Fuzzer Description
-- descriptionF =
--     Fuzz.map IWantTo Fuzz.string
-- reasonF : Fuzzer Reason
-- reasonF =
--     Fuzz.map SoThat Fuzz.string
-- accountF : Fuzzer Account
-- accountF =
--     Fuzz.oneOf [ patronF, anonymousF ]
-- patronF : Fuzzer Account
-- patronF =
--     Fuzz.map Patron nameF
-- anonymousF : Fuzzer Account
-- anonymousF =
--     Fuzz.constant Anonymous
-- nameF : Fuzzer Name
-- nameF =
--     Fuzz.map Named Fuzz.string
-- ideaF : Fuzzer Idea
-- ideaF =
--     Fuzz.map3 AsA patronF descriptionF reasonF
-- posixF : Fuzzer Posix
-- posixF =
--     Fuzz.map Time.millisToPosix Fuzz.int
-- magicF : Fuzzer Magic
-- magicF =
--     Fuzz.oneOf <| List.map Fuzz.constant [ ruby, php, oop, devOps, editCode, git ]
-- masteryF : Fuzzer Mastery
-- masteryF =
--     Fuzz.map MasterOf magicF
-- predictionF : Fuzzer Prediction
-- predictionF =
--     Fuzz.oneOf <| List.map Fuzz.constant [ One, Two, Three, Five, Eight, Thirteen ]
-- philosophyF : Fuzzer Philosophy
-- philosophyF =
--     Fuzz.map2 GuessOf predictionF masteryF
-- incantationF : Fuzzer Incantation
-- incantationF =
--     Fuzz.map2 Incantation ideaF philosophyF
-- pipelineF : Fuzzer Pipeline
-- pipelineF =
--     Fuzz.map Pipeline futureF
-- futureF : Fuzzer (List Summons)
-- futureF =
--     Fuzz.list summonsF
-- summonsF : Fuzzer Summons
-- summonsF =
--     Fuzz.map2 Summons questF fellowshipF
-- questF : Fuzzer Quest
-- questF =
--     Fuzz.map Quest (Fuzz.list <| incantationF)
-- fellowshipF : Fuzzer Fellowship
-- fellowshipF =
--     Fuzz.map Fellowship (Fuzz.list <| sourcererF)
-- taleF : Fuzzer Past
-- taleF =
--     Fuzz.map Past (Fuzz.list deedF)
-- deedF : Fuzzer Deed
-- deedF =
--     Fuzz.map2 Deed predictionF posixF
-- prowessF : Fuzzer Prowess
-- prowessF =
--     Fuzz.oneOf <| List.map Fuzz.constant [ ruby, php, oop, devOps, editCode, git ]
-- sourcererF : Fuzzer Sourcerer
-- sourcererF =
--     Fuzz.map Sourcerer <| Fuzz.map5
--         (Fuzz.maybe incantationF)
--         nameF
--         pipelineF
--         taleF
--         prowessF


patron1 : Person
patron1 =
    Generous joeId


joeId : Patron.Id
joeId =
    Tuple.first joe


joe : ( Patron.Id, Patron.Table )
joe =
    Patron.new { name = Name "Joe" } Dict.empty


dom : ( Sourcerer.Id, Sourcerer.Table )
dom =
    Sourcerer.new
        domInfo
        Dict.empty


domInfo : Sourcerer.Info
domInfo =
    { name = Name "Dominic Serrano"
    , tale = fourteenPointsPerJourney
    , skills = domsProwess
    }


domsId : Sourcerer.Id
domsId =
    Tuple.first dom


domsProwess : Prowess
domsProwess =
    [ ruby, php ]
        |> List.map Tuple.first
        |> Set.fromList
        |> Skill


austin : ( Sourcerer.Id, Sourcerer.Table )
austin =
    Sourcerer.new
        { name = Name "Austin Erlandson"
        , tale = fifteenPointsPerJourney
        , skills = austinsProwess magic
        }
        Dict.empty


austinsId : Sourcerer.Id
austinsId =
    Tuple.first austin


austinsProwess : Magic.Table -> Prowess
austinsProwess =
    Dict.keys
        >> List.take 2
        >> Set.fromList
        >> Skill


shortPast : Past
shortPast =
    [ ( One, cal 2018 Oct 22 )
    ]


fifteenPointsPerJourney : Past
fifteenPointsPerJourney =
    -- Should avg to 15 points
    -- 15 Points
    [ ( Five, cal 2018 Oct 23 )
    , ( One, cal 2018 Oct 22 )
    , ( Three, cal 2018 Oct 21 )
    , ( One, cal 2018 Oct 20 )

    -- 15 Points
    , ( Five, cal 2018 Oct 19 )
    , ( One, cal 2017 Oct 21 )
    , ( Three, cal 2017 Oct 21 )
    , ( Eight, cal 2017 Oct 21 )
    , ( Three, cal 2017 Oct 21 )
    ]


fourteenPointsPerJourney : Past
fourteenPointsPerJourney =
    -- Should avg to 15 points
    [ ( One, cal 2018 Jan 1 )

    -- 15 Points
    , ( Five, cal 2018 Oct 23 )
    , ( One, cal 2018 Oct 22 )
    , ( Three, cal 2018 Oct 21 )
    , ( One, cal 2018 Oct 20 )
    , ( Five, cal 2018 Oct 19 )

    -- 15 Points
    , ( One, cal 2017 Oct 21 )
    , ( Three, cal 2017 Oct 21 )
    , ( Eight, cal 2017 Oct 21 )
    , ( Three, cal 2017 Oct 21 )
    ]


middleEarth : World
middleEarth =
    { incantations = incantations
    , magic = magic
    , patrons = Dict.empty
    , summons = summons
    , sourcerers = sourcerers
    }


incantations : Incantation.Table
incantations =
    incantationPairs
        |> List.map Tuple.second
        |> List.foldl Dict.union Dict.empty


incantationIds : List Incantation.Id
incantationIds =
    List.map Tuple.first incantationPairs


incantationPairs : List ( Incantation.Id, Incantation.Table )
incantationPairs =
    [ incantation1
    , incantation2
    , incantation3
    , incantation4
    , incantation5
    , incantation6
    , incantation7
    ]


incantation1Info : Incantation.Info
incantation1Info =
    { idea =
        AsA patron1
            (IWantTo "change the color of the button to blue")
            (SoThat "it is consistent with the site theme.")
    , effort = GuessOf One gitAndCodeMastery
    }


incantation1 : ( Incantation.Id, Incantation.Table )
incantation1 =
    Incantation.new
        incantation1Info
        Dict.empty


incantation2Info : Incantation.Info
incantation2Info =
    { idea =
        AsA patron1
            (IWantTo "build an index page for Users")
            (SoThat "I can see who's using the system")
    , effort = GuessOf Eight rubyAndPhpMastery
    }


incantation2 : ( Incantation.Id, Incantation.Table )
incantation2 =
    Incantation.new
        incantation2Info
        Dict.empty


incantation3 : ( Incantation.Id, Incantation.Table )
incantation3 =
    Incantation.new
        { idea =
            AsA patron1
                (IWantTo "update the color of the button")
                (SoThat "it is consistent with the theme")
        , effort = GuessOf Two gitAndCodeMastery
        }
        Dict.empty


incantation4 : ( Incantation.Id, Incantation.Table )
incantation4 =
    Incantation.new
        { idea =
            AsA patron1
                (IWantTo "implement search")
                (SoThat "I can find what I'm looking for.")
        , effort = GuessOf Thirteen gitAndCodeMastery
        }
        Dict.empty


incantation5 : ( Incantation.Id, Incantation.Table )
incantation5 =
    Incantation.new
        { idea =
            AsA patron1
                (IWantTo "deploy my app")
                (SoThat "I can see it somewhere")
        , effort = GuessOf Thirteen gitAndCodeMastery
        }
        Dict.empty


incantation6 : ( Incantation.Id, Incantation.Table )
incantation6 =
    Incantation.new
        { idea =
            AsA patron1
                (IWantTo "Integrate with 3rd Party Oauth client")
                (SoThat "users can connect their profiles")
        , effort = GuessOf Thirteen gitAndCodeMastery
        }
        Dict.empty


incantation7 : ( Incantation.Id, Incantation.Table )
incantation7 =
    Incantation.new
        { idea =
            AsA patron1
                (IWantTo "change the color of the button to blue")
                (SoThat "it is consistent with the site theme.")
        , effort = GuessOf One gitAndCodeMastery
        }
        Dict.empty


patrons : Patron.Table
patrons =
    Tuple.second joe


summons : Summons.Table
summons =
    List.foldl Dict.union Dict.empty <|
        List.map Tuple.second
            [ shortSummonsNoFellowsPair
            , secondSummons
            ]


fiftyPointSummonsId : Summons.Id
fiftyPointSummonsId =
    Tuple.first fiftyPointSummonsPair


fiftyPointSummons : Summons.Table
fiftyPointSummons =
    Tuple.second fiftyPointSummonsPair


fiftyPointSummonsPair : ( Summons.Id, Summons.Table )
fiftyPointSummonsPair =
    Summons.new
        { quest = fiftyPointQuest
        , fellowship = fellowship1
        , patronId = joeId
        }
        Dict.empty


fiftyPointSummonsTwoFellowsId : Summons.Id
fiftyPointSummonsTwoFellowsId =
    Tuple.first fiftyPointSummonsTwoFellowsPair


fiftyPointSummonsTwoFellows : Summons.Table
fiftyPointSummonsTwoFellows =
    Tuple.second fiftyPointSummonsTwoFellowsPair


fiftyPointSummonsTwoFellowsPair : ( Summons.Id, Summons.Table )
fiftyPointSummonsTwoFellowsPair =
    Summons.new
        { quest = fiftyPointQuest
        , fellowship = fellowship2
        , patronId = joeId
        }
        Dict.empty


emptySummons : Summons.Table
emptySummons =
    Dict.empty


shortSummonsNoFellows : Summons.Table
shortSummonsNoFellows =
    Tuple.second shortSummonsNoFellowsPair


shortSummonsNoFellowsPair : ( Summons.Id, Summons.Table )
shortSummonsNoFellowsPair =
    Summons.new
        { quest = quest1
        , fellowship = emptyFellowship
        , patronId = joeId
        }
        Dict.empty


shortSummonsNoFellowsId : Summons.Id
shortSummonsNoFellowsId =
    Tuple.first shortSummonsNoFellowsPair


summonsNoQuestOneFellow : Summons.Table
summonsNoQuestOneFellow =
    Tuple.second summonsNoQuestOneFellowPair


summonsNoQuestOneFellowPair : ( Summons.Id, Summons.Table )
summonsNoQuestOneFellowPair =
    Summons.new
        { quest = emptyQuest
        , fellowship = fellowship1
        , patronId = joeId
        }
        Dict.empty


summonsNoQuestOneFellowId : Summons.Id
summonsNoQuestOneFellowId =
    Tuple.first summonsNoQuestOneFellowPair


firstSummons : ( Summons.Id, Summons.Table )
firstSummons =
    Summons.new
        { quest = quest1
        , fellowship = fellowship1
        , patronId = joeId
        }
        Dict.empty


quest1 : Quest
quest1 =
    Set.fromList <| List.map Tuple.first [ incantation1 ]


quest2 : Quest
quest2 =
    Set.fromList <| List.map Tuple.first [ incantation2 ]


fiftyPointQuest : Quest
fiftyPointQuest =
    Set.fromList <| List.drop 1 incantationIds


emptyQuest : Quest
emptyQuest =
    Set.empty


fellowship1 : Fellow.Ship
fellowship1 =
    Set.fromList [ austinsId ]


fellowship2 : Fellow.Ship
fellowship2 =
    Set.fromList [ austinsId, domsId ]


emptyFellowship : Fellow.Ship
emptyFellowship =
    Set.empty


firstSummonsId : Summons.Id
firstSummonsId =
    Tuple.first firstSummons


secondSummons : ( Summons.Id, Summons.Table )
secondSummons =
    Summons.new
        { quest = quest2
        , fellowship = fellowship2
        , patronId = joeId
        }
        Dict.empty


secondSummonsId : Summons.Id
secondSummonsId =
    Tuple.first secondSummons


sourcerers : Sourcerer.Table
sourcerers =
    List.foldl Dict.union Dict.empty <|
        List.map Tuple.second [ austin, dom ]



{-
   Magic
-}


magic : Magic.Table
magic =
    List.foldl Dict.union Dict.empty <|
        List.map
            Tuple.second
            [ ruby
            , php
            , oop
            , devOps
            , editCode
            , git
            ]


ruby : ( Magic.Id, Magic.Table )
ruby =
    Magic.new { spell = "ruby" } Dict.empty


php : ( Magic.Id, Magic.Table )
php =
    Magic.new { spell = "PHP" } Dict.empty


oop : ( Magic.Id, Magic.Table )
oop =
    Magic.new { spell = "Object Oriented Web Languages" } Dict.empty


devOps : ( Magic.Id, Magic.Table )
devOps =
    Magic.new { spell = "DevOps" } Dict.empty


editCode : ( Magic.Id, Magic.Table )
editCode =
    Magic.new { spell = "edit code" } Dict.empty


git : ( Magic.Id, Magic.Table )
git =
    Magic.new { spell = "git" } Dict.empty



{-
   Mastery
-}


gitMastery : Mastery
gitMastery =
    MasterOf <| Set.singleton <| Tuple.first git


gitAndCodeMastery : Mastery
gitAndCodeMastery =
    MasterOf <| Set.fromList <| List.map Tuple.first [ editCode, git ]


rubyAndPhpMastery : Mastery
rubyAndPhpMastery =
    MasterOf <| Set.fromList <| List.map Tuple.first [ ruby, php ]
