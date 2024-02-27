module Data.Textile.InputsTest exposing (..)

import Data.Country as Country
import Data.Split as Split
import Data.Textile.Inputs as Inputs exposing (jupeCotonAsie, tShirtCotonAsie, tShirtCotonFrance)
import Data.Textile.LifeCycle as LifeCycle
import Data.Textile.Material as Material
import Data.Textile.Product as Product
import Data.Textile.Simulator as Simulator
import Data.Textile.Step.Label as Label
import Data.Unit as Unit
import Expect
import List.Extra as LE
import Quantity
import Test exposing (..)
import TestUtils exposing (asTest, suiteWithDb)


sampleQuery : Inputs.Query
sampleQuery =
    { jupeCotonAsie
        | materials = [ { id = Material.Id "acrylique", share = Split.full, spinning = Nothing, country = Just (Country.Code "CN") } ]
    }


suite : Test
suite =
    suiteWithDb "Data.Inputs"
        (\db ->
            [ describe "Base64"
                [ describe "Encoding and decoding queries"
                    [ sampleQuery
                        |> Inputs.fromQuery db
                        |> Result.map Inputs.toQuery
                        |> Expect.equal (Ok sampleQuery)
                        |> asTest "should encode and decode a query"
                    ]
                , describe "Base64 encoding and decoding queries"
                    [ sampleQuery
                        |> Inputs.b64encode
                        |> Inputs.b64decode
                        |> Expect.equal (Ok sampleQuery)
                        |> asTest "should base64 encode and decode a query"
                    ]
                ]
            , describe "Query countries validation"
                [ { tShirtCotonAsie
                    | countryFabric = Country.Code "CN"
                    , countryDyeing = Country.Code "CN"
                    , countryMaking = Country.Code "CN"
                  }
                    |> Inputs.fromQuery db
                    |> Result.map Inputs.countryList
                    |> Result.andThen (LE.getAt 0 >> Maybe.map .code >> Result.fromMaybe "")
                    |> Expect.equal (Ok (Country.codeFromString "CN"))
                    |> asTest "should replace the first country with the material's default country"
                , { tShirtCotonAsie
                    | countryFabric = Country.Code "XX"
                    , countryDyeing = Country.Code "CN"
                    , countryMaking = Country.Code "CN"
                  }
                    |> Inputs.fromQuery db
                    |> Expect.equal (Err "Code pays invalide: XX.")
                    |> asTest "should validate country codes"
                ]
            , describe "Product update"
                [ asTest "should update step masses"
                    (case Product.findById (Product.Id "jean") db.textile.products of
                        Ok jean ->
                            tShirtCotonAsie
                                |> Inputs.updateProduct jean
                                |> Simulator.compute db
                                |> Result.map (.lifeCycle >> LifeCycle.getStepProp Label.Distribution .inputMass Quantity.zero)
                                |> Expect.equal (Ok jean.mass)

                        Err error ->
                            Expect.fail error
                    )
                ]
            , let
                testComplementEqual x =
                    Inputs.fromQuery db
                        >> Result.map (Inputs.getOutOfEuropeEOLComplement >> Unit.impactToFloat)
                        >> Result.withDefault 0
                        >> Expect.within (Expect.Absolute 0.001) x
              in
              describe "getOutOfEuropeEOLComplement"
                [ tShirtCotonFrance
                    |> testComplementEqual -42.5
                    |> asTest "should compute OutOfEuropeEOL complement impact for a fully natural garment"
                , { tShirtCotonFrance
                    | materials =
                        [ { id = Material.Id "coton", share = Split.half, spinning = Nothing, country = Nothing }
                        , { id = Material.Id "pu", share = Split.half, spinning = Nothing, country = Nothing }
                        ]
                  }
                    |> testComplementEqual -102
                    |> asTest "should compute OutOfEuropeEOL complement impact for a half-natural, half-synthetic garment"
                ]
            , let
                testComplementEqual x =
                    Inputs.fromQuery db
                        >> Result.map (Inputs.getTotalMicrofibersComplement >> Unit.impactToFloat)
                        >> Result.withDefault 0
                        >> Expect.within (Expect.Absolute 0.001) x
              in
              describe "getMicrofibersComplement"
                [ tShirtCotonFrance
                    |> testComplementEqual -42.5
                    |> asTest "should compute Microfibers complement impact for a fully natural garment"
                , { tShirtCotonFrance
                    | materials =
                        [ { id = Material.Id "coton", share = Split.half, spinning = Nothing, country = Nothing }
                        , { id = Material.Id "pu", share = Split.half, spinning = Nothing, country = Nothing }
                        ]
                  }
                    |> testComplementEqual -90.95
                    |> asTest "should compute Microfibers complement impact for a half-natural, half-synthetic garment"
                ]
            , Inputs.productsAndNames
                |> List.map
                    (\{ name, query } ->
                        Expect.ok (Inputs.fromQuery db query)
                            |> asTest (name ++ " example is ok")
                    )
                |> describe "productsAndNames"
            ]
        )
