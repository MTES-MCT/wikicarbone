module Data.Textile.Inputs exposing
    ( Inputs
    , MaterialInput
    , computeMaterialTransport
    , countryList
    , encode
    , fromQuery
    , getMaterialMicrofibersComplement
    , getOutOfEuropeEOLComplement
    , getOutOfEuropeEOLProbability
    , getTotalMicrofibersComplement
    , isFabricOfType
    , toQuery
    , toString
    )

import Data.Component exposing (Item)
import Data.Country as Country exposing (Country)
import Data.Impact as Impact
import Data.Process as Process
import Data.Split as Split exposing (Split)
import Data.Textile.Dyeing as Dyeing exposing (ProcessType)
import Data.Textile.Economics as Economics
import Data.Textile.Fabric as Fabric exposing (Fabric)
import Data.Textile.MakingComplexity as MakingComplexity exposing (MakingComplexity)
import Data.Textile.Material as Material exposing (Material)
import Data.Textile.Material.Origin as Origin exposing (Origin)
import Data.Textile.Material.Spinning as Spinning exposing (Spinning)
import Data.Textile.Printing as Printing exposing (Printing)
import Data.Textile.Product as Product exposing (Product)
import Data.Textile.Query exposing (MaterialQuery, Query)
import Data.Textile.Step.Label as Label exposing (Label)
import Data.Textile.WellKnown exposing (WellKnown)
import Data.Transport as Transport exposing (Distances, Transport)
import Data.Unit as Unit
import Json.Encode as Encode
import Mass exposing (Mass)
import Quantity
import Result.Extra as RE
import Static.Db exposing (Db)
import Views.Format as Format


type alias MaterialInput =
    { country : Maybe Country
    , material : Material
    , share : Split
    , spinning : Maybe Spinning
    }


type alias Inputs =
    { airTransportRatio : Maybe Split
    , business : Maybe Economics.Business
    , countryDistribution : Country
    , countryDyeing : Country
    , countryEndOfLife : Country
    , countryFabric : Country
    , countryMaking : Country

    -- TODO: countryMaterial isn't used anymore, but we still need it because `countryList` uses it,
    -- which in turn is used to build the lifecycle, which needs a country for each step.
    , countryMaterial : Country
    , countrySpinning : Country
    , countryUse : Country
    , disabledSteps : List Label
    , dyeingProcessType : Maybe ProcessType
    , fabricProcess : Maybe Fabric
    , fading : Maybe Bool
    , makingComplexity : Maybe MakingComplexity
    , makingDeadStock : Maybe Split
    , makingWaste : Maybe Split
    , mass : Mass
    , materials : List MaterialInput
    , numberOfReferences : Maybe Int
    , physicalDurability : Maybe Unit.PhysicalDurability
    , price : Maybe Economics.Price
    , printing : Maybe Printing
    , product : Product
    , surfaceMass : Maybe Unit.SurfaceMass
    , traceability : Maybe Bool
    , trims : List Item
    , upcycled : Bool
    , yarnSize : Maybe Unit.YarnSize
    }


fromMaterialQuery : List Material -> List Country -> List MaterialQuery -> Result String (List MaterialInput)
fromMaterialQuery materials countries =
    List.map
        (\{ country, id, share, spinning } ->
            let
                countryResult =
                    case country of
                        Just countryCode ->
                            Country.findByCode countryCode countries
                                |> Result.map Just

                        Nothing ->
                            Ok Nothing
            in
            Result.map2
                (\material_ country_ ->
                    { country = country_
                    , material = material_
                    , share = share
                    , spinning = spinning
                    }
                )
                (Material.findById id materials)
                countryResult
        )
        >> RE.combine


toMaterialQuery : List MaterialInput -> List MaterialQuery
toMaterialQuery =
    List.map
        (\{ country, material, share, spinning } ->
            { country = country |> Maybe.andThen (.code >> toQueryCountryCode)
            , id = material.id
            , share = share
            , spinning = spinning
            }
        )


getMainMaterial : List MaterialInput -> Result String Material
getMainMaterial =
    List.sortBy (.share >> Split.toFloat)
        >> List.reverse
        >> List.head
        >> Maybe.map .material
        >> Result.fromMaybe "La liste de matières est vide."


getMainMaterialCountry : List Country -> List MaterialInput -> Result String Country
getMainMaterialCountry countries =
    getMainMaterial
        >> Result.andThen
            (\{ defaultCountry } ->
                Country.findByCode defaultCountry countries
            )


fromQuery : Db -> Query -> Result String Inputs
fromQuery { countries, textile } query =
    let
        materials_ =
            query.materials
                |> fromMaterialQuery textile.materials countries

        franceResult =
            Country.findByCode (Country.Code "FR") countries

        unknownCountryResult =
            Country.findByCode Country.unknownCountryCode countries

        mainMaterialCountry =
            materials_
                |> Result.andThen (getMainMaterialCountry countries)
                |> RE.orElse unknownCountryResult

        getCountryResult fallbackResult maybeCode =
            case maybeCode of
                Just code ->
                    Country.findByCode code countries

                Nothing ->
                    fallbackResult
    in
    Ok Inputs
        |> RE.andMap (Ok query.airTransportRatio)
        |> RE.andMap (Ok query.business)
        -- The distribution country is always France
        |> RE.andMap franceResult
        |> RE.andMap (getCountryResult unknownCountryResult query.countryDyeing)
        -- The end of life country is always France
        |> RE.andMap franceResult
        |> RE.andMap (getCountryResult unknownCountryResult query.countryFabric)
        |> RE.andMap (getCountryResult unknownCountryResult query.countryMaking)
        -- Material country is constrained to be the first material's default country
        |> RE.andMap mainMaterialCountry
        -- Spinning country is either provided by query or fallbacks to material's default
        -- country, making the parameter optional
        |> RE.andMap (getCountryResult mainMaterialCountry query.countrySpinning)
        -- The use country is always France
        |> RE.andMap franceResult
        |> RE.andMap (Ok query.disabledSteps)
        |> RE.andMap (Ok query.dyeingProcessType)
        |> RE.andMap (Ok query.fabricProcess)
        |> RE.andMap (Ok query.fading)
        |> RE.andMap (Ok query.makingComplexity)
        |> RE.andMap (Ok query.makingDeadStock)
        |> RE.andMap (Ok query.makingWaste)
        |> RE.andMap (Ok query.mass)
        |> RE.andMap materials_
        |> RE.andMap (Ok query.numberOfReferences)
        |> RE.andMap (Ok query.physicalDurability)
        |> RE.andMap (Ok query.price)
        |> RE.andMap (Ok query.printing)
        |> RE.andMap (textile.products |> Product.findById query.product)
        |> RE.andMap (Ok query.surfaceMass)
        |> RE.andMap (Ok query.traceability)
        |> RE.andMap (Ok query.trims)
        |> RE.andMap (Ok query.upcycled)
        |> RE.andMap (Ok query.yarnSize)


toQuery : Inputs -> Query
toQuery inputs =
    { airTransportRatio = inputs.airTransportRatio
    , business = inputs.business
    , countryDyeing = toQueryCountryCode inputs.countryDyeing.code
    , countryFabric = toQueryCountryCode inputs.countryFabric.code
    , countryMaking = toQueryCountryCode inputs.countryMaking.code
    , countrySpinning = toQueryCountryCode inputs.countrySpinning.code
    , disabledSteps = inputs.disabledSteps
    , dyeingProcessType = inputs.dyeingProcessType
    , fabricProcess = inputs.fabricProcess
    , fading = inputs.fading
    , makingComplexity = inputs.makingComplexity
    , makingDeadStock = inputs.makingDeadStock
    , makingWaste = inputs.makingWaste
    , mass = inputs.mass
    , materials = toMaterialQuery inputs.materials
    , numberOfReferences = inputs.numberOfReferences
    , physicalDurability = inputs.physicalDurability
    , price = inputs.price
    , printing = inputs.printing
    , product = inputs.product.id
    , surfaceMass = inputs.surfaceMass
    , traceability = inputs.traceability
    , trims = inputs.trims
    , upcycled = inputs.upcycled
    , yarnSize = inputs.yarnSize
    }


toQueryCountryCode : Country.Code -> Maybe Country.Code
toQueryCountryCode c =
    if c == Country.unknownCountryCode then
        Nothing

    else
        Just c


stepsToStrings : WellKnown -> Inputs -> List (List String)
stepsToStrings wellKnown inputs =
    let
        ifStepEnabled label list =
            if not (List.member label inputs.disabledSteps) then
                list

            else
                []
    in
    [ [ inputs.product.name
            ++ (if inputs.upcycled then
                    " remanufacturé"

                else
                    ""
               )
            ++ (case inputs.physicalDurability of
                    Just physicalDurability ->
                        ", durabilité physique " ++ String.fromFloat (Unit.physicalDurabilityToFloat physicalDurability)

                    Nothing ->
                        ""
               )
      , Format.kgToString inputs.mass
      ]
    , ifStepEnabled Label.Material
        [ "matière"
        , materialsToString inputs.materials
        ]
    , ifStepEnabled Label.Spinning
        [ "filature"
        , inputs.countrySpinning.name
        ]
    , case inputs.yarnSize of
        Just yarnSize ->
            [ "titrage", String.fromFloat (Unit.yarnSizeInKilometers yarnSize) ++ "Nm" ]

        Nothing ->
            []
    , ifStepEnabled Label.Fabric
        [ inputs.fabricProcess
            |> Maybe.withDefault inputs.product.fabric
            |> Fabric.toLabel
        , inputs.countryFabric.name
        ]
    , ifStepEnabled Label.Ennobling
        [ "ennoblissement\u{00A0}: "
            ++ (inputs.dyeingProcessType
                    |> Dyeing.toProcess wellKnown
                    |> Process.getDisplayName
               )
        , inputs.countryDyeing.name
        ]
    , ifStepEnabled Label.Ennobling
        [ "impression"
        , case inputs.printing of
            Just printing ->
                "impression " ++ Printing.toFullLabel printing ++ "\u{00A0}: " ++ inputs.countryDyeing.name

            Nothing ->
                "non"
        ]
    , ifStepEnabled Label.Making
        [ "confection"
        , inputs.countryMaking.name ++ makingOptionsToString inputs
        ]
    , ifStepEnabled Label.Distribution
        [ "distribution"
        , inputs.countryDistribution.name
        ]
    , ifStepEnabled Label.Use
        [ "utilisation"
        , inputs.countryUse.name
        ]
    , ifStepEnabled Label.EndOfLife
        [ "fin de vie"
        , inputs.countryEndOfLife.name
        ]
    ]
        |> List.filter (not << List.isEmpty)


toString : WellKnown -> Inputs -> String
toString wellKnown inputs =
    inputs
        |> stepsToStrings wellKnown
        |> List.map (String.join "\u{00A0}: ")
        |> String.join ", "


materialsToString : List MaterialInput -> String
materialsToString materials =
    materials
        |> List.filter (\{ share } -> Split.toFloat share > 0)
        |> List.map
            (\{ country, material, share } ->
                let
                    countryName =
                        country
                            |> Maybe.map .name
                            |> Maybe.withDefault (" par défaut (" ++ material.geographicOrigin ++ ")")
                in
                Split.toPercentString 0 share
                    ++ "% "
                    ++ material.shortName
                    ++ " provenance "
                    ++ countryName
            )
        |> String.join ", "


makingOptionsToString : Inputs -> String
makingOptionsToString { airTransportRatio, fading, makingComplexity, makingDeadStock, makingWaste } =
    [ makingWaste
        |> Maybe.map (Split.toPercentString 0 >> (\s -> s ++ "\u{202F}% de perte"))
    , makingDeadStock
        |> Maybe.map (Split.toPercentString 0 >> (\s -> s ++ "\u{202F}% de stocks dormants"))
    , makingComplexity
        |> Maybe.map (\complexity -> "complexité de confection " ++ MakingComplexity.toLabel complexity)
    , airTransportRatio
        |> Maybe.andThen
            (\ratio ->
                if Split.toPercent ratio == 0 then
                    Nothing

                else
                    Just (Split.toPercentString 0 ratio ++ " de transport aérien")
            )
    , if fading == Just True then
        Just "délavé"

      else
        Nothing
    ]
        |> List.filterMap identity
        |> String.join ", "
        |> (\s ->
                if s /= "" then
                    " (" ++ s ++ ")"

                else
                    ""
           )


countryList : Inputs -> List Country
countryList inputs =
    [ inputs.countryMaterial
    , inputs.countrySpinning
    , inputs.countryFabric
    , inputs.countryDyeing
    , inputs.countryMaking
    , inputs.countryDistribution
    , inputs.countryUse
    , inputs.countryEndOfLife
    ]


getMaterialMicrofibersComplement : Mass -> MaterialInput -> Unit.Impact
getMaterialMicrofibersComplement finalProductMass { material, share } =
    -- Note: Impact is computed against the final product mass, because microfibers
    --       are always released in the environment from finished products.
    let
        materialMassInKg =
            share
                |> Split.applyToQuantity finalProductMass
                |> Mass.inKilograms
    in
    Origin.toMicrofibersComplement material.origin
        |> Quantity.multiplyBy materialMassInKg


getTotalMicrofibersComplement : Inputs -> Unit.Impact
getTotalMicrofibersComplement { mass, materials } =
    materials
        |> List.map (getMaterialMicrofibersComplement mass)
        |> Quantity.sum


getMaterialCategoryShare : Origin -> List MaterialInput -> Split
getMaterialCategoryShare origin =
    List.filterMap
        (\{ material, share } ->
            if material.origin == origin then
                Just (Split.toPercent share)

            else
                Nothing
        )
        >> List.sum
        >> Split.fromPercent
        >> Result.withDefault Split.zero


getOutOfEuropeEOLProbability : List MaterialInput -> Split
getOutOfEuropeEOLProbability materialInputs =
    -- We consider that the garment enters the "synthetic materials" category as
    -- soon as synthetic materials represent more than 10% of its composition.
    let
        syntheticMaterialsShare =
            materialInputs
                |> getMaterialCategoryShare Origin.Synthetic
    in
    Split.fromFloat
        (if Split.toPercent syntheticMaterialsShare >= 50 then
            0.121

         else
            0.049
        )
        |> Result.withDefault Split.zero


getOutOfEuropeEOLComplement : Inputs -> Unit.Impact
getOutOfEuropeEOLComplement { mass, materials } =
    -- Note: this complement is a malus, hence the minus sign
    Unit.impact
        -(Split.toFloat (getOutOfEuropeEOLProbability materials)
            * Mass.inKilograms mass
            * 5000
         )


computeMaterialTransport : Distances -> Country.Code -> MaterialInput -> Transport
computeMaterialTransport distances nextCountryCode { country, material, share } =
    if share /= Split.zero then
        let
            emptyImpacts =
                Impact.empty

            countryCode =
                country
                    |> Maybe.map .code
                    |> Maybe.withDefault material.defaultCountry
        in
        distances
            |> Transport.getTransportBetween emptyImpacts countryCode nextCountryCode

    else
        Transport.default Impact.empty


isFabricOfType : Fabric -> Inputs -> Bool
isFabricOfType fabric { fabricProcess, product } =
    fabric == Maybe.withDefault product.fabric fabricProcess


encode : Inputs -> Encode.Value
encode inputs =
    Encode.object
        [ ( "airTransportRatio", inputs.airTransportRatio |> Maybe.map Split.encodeFloat |> Maybe.withDefault Encode.null )
        , ( "business", inputs.business |> Maybe.map Economics.encodeBusiness |> Maybe.withDefault Encode.null )
        , ( "countryDyeing", Country.encode inputs.countryDyeing )
        , ( "countryFabric", Country.encode inputs.countryFabric )
        , ( "countryMaking", Country.encode inputs.countryMaking )
        , ( "disabledSteps", Encode.list Label.encode inputs.disabledSteps )
        , ( "dyeingProcessType", inputs.dyeingProcessType |> Maybe.map Dyeing.encode |> Maybe.withDefault Encode.null )
        , ( "fabricProcess", inputs.fabricProcess |> Maybe.map Fabric.encode |> Maybe.withDefault Encode.null )
        , ( "fading", inputs.fading |> Maybe.map Encode.bool |> Maybe.withDefault Encode.null )
        , ( "makingComplexity", inputs.makingComplexity |> Maybe.map (MakingComplexity.toString >> Encode.string) |> Maybe.withDefault Encode.null )
        , ( "makingDeadStock", inputs.makingDeadStock |> Maybe.map Split.encodeFloat |> Maybe.withDefault Encode.null )
        , ( "makingWaste", inputs.makingWaste |> Maybe.map Split.encodeFloat |> Maybe.withDefault Encode.null )
        , ( "mass", Encode.float (Mass.inKilograms inputs.mass) )
        , ( "materials", Encode.list encodeMaterialInput inputs.materials )
        , ( "numberOfReferences", inputs.numberOfReferences |> Maybe.map Encode.int |> Maybe.withDefault Encode.null )
        , ( "physicalDurability", inputs.physicalDurability |> Maybe.map Unit.encodePhysicalDurability |> Maybe.withDefault Encode.null )
        , ( "price", inputs.price |> Maybe.map Economics.encodePrice |> Maybe.withDefault Encode.null )
        , ( "printing", inputs.printing |> Maybe.map Printing.encode |> Maybe.withDefault Encode.null )
        , ( "product", Product.encode inputs.product )
        , ( "surfaceMass", inputs.surfaceMass |> Maybe.map Unit.encodeSurfaceMass |> Maybe.withDefault Encode.null )
        , ( "traceability", inputs.traceability |> Maybe.map Encode.bool |> Maybe.withDefault Encode.null )
        , ( "upcycled", Encode.bool inputs.upcycled )
        , ( "yarnSize", inputs.yarnSize |> Maybe.map Unit.encodeYarnSize |> Maybe.withDefault Encode.null )
        ]


encodeMaterialInput : MaterialInput -> Encode.Value
encodeMaterialInput v =
    [ ( "country", v.country |> Maybe.map (.code >> Country.encodeCode) )
    , ( "material", Material.encode v.material |> Just )
    , ( "share", Split.encodeFloat v.share |> Just )
    , ( "spinning", v.spinning |> Maybe.map Spinning.encode )
    ]
        |> List.filterMap (\( key, maybeVal ) -> maybeVal |> Maybe.map (\val -> ( key, val )))
        |> Encode.object
