module TestUtils exposing
    ( asTest
    , expectImpactsEqual
    , suiteWithDb
    )

import Data.Impact as Impact exposing (Impacts)
import Data.Impact.Definition as Definition exposing (Trigrams)
import Data.Unit as Unit
import Expect exposing (Expectation)
import Static.Db as StaticDb exposing (Db)
import Static.Json as StaticJson
import Test exposing (..)


asTest : String -> Expectation -> Test
asTest label =
    always >> test label


suiteWithDb : String -> (Db -> List Test) -> Test
suiteWithDb name suite =
    case StaticDb.db StaticJson.rawJsonProcesses of
        Ok db ->
            describe name (suite db)

        Err error ->
            describe name
                [ test "should load static database" <|
                    \_ -> Expect.fail <| "Couldn't parse static database: " ++ error
                ]


expectImpactsEqual : Trigrams (Float -> Expectation) -> Impacts -> Expectation
expectImpactsEqual impacts subject =
    Definition.trigrams
        |> List.map
            (\trigram ->
                Impact.getImpact trigram >> Unit.impactToFloat >> Definition.get trigram impacts
            )
        |> (\expectations ->
                Expect.all expectations subject
           )
