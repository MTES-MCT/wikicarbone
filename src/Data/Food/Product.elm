module Data.Food.Product exposing
    ( Amount
    , Item
    , Product
    , ProductName
    , Products
    , Step
    , addMaterial
    , computeItemPefImpact
    , decodeProducts
    , defaultCountry
    , emptyProducts
    , findProductByName
    , formatItem
    , getAmountRatio
    , getMainItemComment
    , getStepImpact
    , getStepTransports
    , getTotalImpact
    , getWeightAtPlant
    , getWeightAtStep
    , listIngredientProcesses
    , listIngredients
    , listProcessingProcesses
    , productNameToString
    , removeMaterial
    , stepToItems
    , stringToProductName
    , updateMaterialAmount
    , updatePlantTransport
    )

import Data.Country as Country
import Data.Food.Process as Process exposing (Process, ProcessName)
import Data.Impact as Impact
import Data.Textile.Formula as Formula
import Data.Transport as Transport exposing (Distances)
import Data.Unit as Unit
import Dict exposing (Dict)
import Dict.Any as AnyDict exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DE
import Json.Decode.Pipeline as Pipe
import Length exposing (Length)
import List.Extra as LE
import Quantity
import Views.Format as Format


defaultCountry : Country.Code
defaultCountry =
    Country.codeFromString "FR"


formatAmount : Float -> String -> Float -> String
formatAmount totalWeight unit amount =
    if unit == "t/km" then
        let
            -- amount is in Ton.Km for the total weight. We instead want the total number of km.
            perKg =
                amount / totalWeight

            distanceInKm =
                perKg * 1000
        in
        Format.formatFloat 0 distanceInKm
            ++ "\u{00A0}km ("
            ++ Format.formatFloat 2 (amount * 1000)
            ++ "\u{00A0}kg.km)"

    else
        Format.formatFloat 2 amount ++ "\u{00A0}" ++ unit


formatItem : Float -> Item -> String
formatItem totalWeight item =
    formatAmount totalWeight item.process.unit item.amount


{-| Item
An item is one entry from one category (transport, material, processing...)
from one step (consumer, packaging, plant...)
from one product from public/data/products.json
It links a Process to an amount for this process (quantity of a vegetable, transport distance, ...)
-}
type alias Amount =
    Float


type alias Item =
    { amount : Amount
    , comment : String
    , process : Process
    }


type alias Items =
    List Item


computeItemsPefImpact : List Impact.Definition -> Items -> Items
computeItemsPefImpact definitions items =
    items
        |> List.map (computeItemPefImpact definitions)


computeItemPefImpact : List Impact.Definition -> { a | process : Process } -> { a | process : Process }
computeItemPefImpact definitions ({ process } as item) =
    { item
        | process =
            { process
                | impacts =
                    Impact.updatePefImpact definitions process.impacts
            }
    }


{-| Step
A step (at consumer, at plant...) has several categories (material, transport...) containing several items
A Product is composed of several steps.
-}
type alias Step =
    { mainItem : Item
    , items : Items
    }


type alias Product =
    { consumer : Step
    , supermarket : Step
    , distribution : Step
    , packaging : Step
    , plant : Items
    }


type ProductName
    = ProductName String


productNameToString : ProductName -> String
productNameToString (ProductName name) =
    name


stringToProductName : String -> ProductName
stringToProductName str =
    ProductName str


type alias Products =
    AnyDict String ProductName Product


emptyProducts : Products
emptyProducts =
    AnyDict.empty productNameToString


findProductByName : ProductName -> Products -> Result String Product
findProductByName ((ProductName name) as productName) =
    AnyDict.get productName
        >> Result.fromMaybe ("Produit introuvable par nom : " ++ name)


decodeAmount : Decoder Amount
decodeAmount =
    Decode.float


linkProcess : List Process -> Decoder Process
linkProcess processes =
    Decode.string
        |> Decode.andThen
            (Process.nameFromString
                >> Process.findByName processes
                >> DE.fromResult
            )


decodeItem : List Process -> Decoder Item
decodeItem processes =
    Decode.succeed Item
        -- FIXME: decodeAmout should be called with the unit decoded from
        -- JSON in decodeProcess, so we could have properly typed values
        |> Pipe.required "amount" decodeAmount
        |> Pipe.required "comment" Decode.string
        |> Pipe.required "processName" (linkProcess processes)


decodeItems : List Process -> Decoder Items
decodeItems processes =
    Decode.list (decodeItem processes)


decodeStep : List Process -> Decoder Step
decodeStep processes =
    Decode.succeed Step
        |> Pipe.required "mainItem" (decodeItem processes)
        |> Pipe.required "items" (decodeItems processes)


decodeProduct : List Process -> Decoder Product
decodeProduct processes =
    Decode.succeed Product
        |> Pipe.required "consumer" (decodeStep processes)
        |> Pipe.required "supermarket" (decodeStep processes)
        |> Pipe.required "distribution" (decodeStep processes)
        |> Pipe.required "packaging" (decodeStep processes)
        |> Pipe.required "plant" (decodeItems processes)


decodeProducts : List Process -> Decoder Products
decodeProducts processes =
    AnyDict.decode (\str _ -> ProductName str) productNameToString (decodeProduct processes)



-- utilities


stepToItems : Step -> Items
stepToItems step =
    -- Return a "flat" list of items
    -- FIXME: find a way to validate that we're using all the important record properties
    [ .transport, .wasteTreatment, .energy, .processing, .material ]
        |> List.concatMap (\accessor -> accessor step)


getStepImpact : Impact.Trigram -> Step -> Float
getStepImpact trigram step =
    step
        |> stepToItems
        |> List.filter (.mainItem >> not)
        |> List.foldl
            (\item total ->
                let
                    impact =
                        Impact.getImpact trigram item.process.impacts
                            |> Unit.impactToFloat
                in
                total + (item.amount * impact)
            )
            0


getMainItemComment : Step -> Maybe String
getMainItemComment step =
    step
        |> stepToItems
        |> List.filter .mainItem
        |> List.head
        |> Maybe.map .comment


getTotalImpact : Impact.Trigram -> Product -> Float
getTotalImpact trigram product =
    getStepImpact trigram product.consumer
        + getStepImpact trigram product.supermarket
        + getStepImpact trigram product.distribution
        + getStepImpact trigram product.packaging
        + getStepImpact trigram product.plant


transportModes : Dict String String
transportModes =
    -- Transport processes, categorized by mode (road, sea, air, train)
    Dict.fromList
        [ ( "Transport, freight, inland waterways, barge {RER}| processing | Cut-off, S - Copied from Ecoinvent", "sea" )
        , ( "Transport, freight, inland waterways, barge with reefer, cooling {GLO}| processing | Cut-off, S - Copied from Ecoinvent", "sea" )
        , ( "Transport, freight, lorry 16-32 metric ton, euro6 {RER}| market for transport, freight, lorry 16-32 metric ton, EURO6 | Cut-off, S - Copied from Ecoinvent", "road" )
        , ( "Transport, freight, lorry >32 metric ton, EURO4 {RER}| transport, freight, lorry >32 metric ton, EURO4 | Cut-off, S - Copied from Ecoinvent", "road" )
        , ( "Transport, freight, lorry 16-32 metric ton, EURO4 {RER}| transport, freight, lorry 16-32 metric ton, EURO4 | Cut-off, S - Copied from Ecoinvent", "road" )
        , ( "Transport, freight, lorry with refrigeration machine, 7.5-16 ton, EURO5, R134a refrigerant, cooling {GLO}| transport, freight, lorry with refrigeration machine, 7.5-16 ton, EURO5, R134a refrigerant, cooling | Cut-off, S - Copied from Ecoinvent", "road" )
        , ( "Transport, freight, lorry 16-32 metric ton, EURO5 {RER}| transport, freight, lorry 16-32 metric ton, EURO5 | Cut-off, S - Copied from Ecoinvent", "road" )
        , ( "Transport, freight train {RER}| market group for transport, freight train | Cut-off, S - Copied from Ecoinvent", "rail" )
        , ( "Transport, freight, sea, transoceanic ship {GLO}| processing | Cut-off, S - Copied from Ecoinvent", "sea" )
        , ( "Transport, freight, sea, transoceanic ship {GLO}| market for | Cut-off, S - Copied from Ecoinvent", "sea" )
        , ( "Transport, freight, sea, transoceanic ship with reefer, cooling {GLO}| processing | Cut-off, S - Copied from Ecoinvent", "sea" )
        , ( "Transport, freight, aircraft {RER}| intercontinental | Cut-off, S - Copied from Ecoinvent", "air" )
        ]


getStepTransports : Step -> { air : Length, rail : Length, road : Length, sea : Length }
getStepTransports step =
    let
        stepWeight =
            getWeightAtStep step
    in
    step
        |> stepToItems
        |> List.foldl
            (\{ amount, process } acc ->
                let
                    distanceToAdd =
                        if process.unit == "t/km" then
                            amount / stepWeight * 1000

                        else
                            amount
                in
                case Dict.get (Process.nameToString process.name) transportModes of
                    Just "air" ->
                        { acc | air = acc.air |> Quantity.plus (Length.kilometers distanceToAdd) }

                    Just "rail" ->
                        { acc | rail = acc.rail |> Quantity.plus (Length.kilometers distanceToAdd) }

                    Just "road" ->
                        { acc | road = acc.road |> Quantity.plus (Length.kilometers distanceToAdd) }

                    Just "sea" ->
                        { acc | sea = acc.sea |> Quantity.plus (Length.kilometers distanceToAdd) }

                    _ ->
                        acc
            )
            { air = Quantity.zero
            , rail = Quantity.zero
            , road = Quantity.zero
            , sea = Quantity.zero
            }


getWeightAtStep : Step -> Float
getWeightAtStep step =
    -- At any given step (that's not "at plant"), we take the first main item we find, and use its
    -- weight to know how much we transport from the previous step.
    step.material
        |> List.filter .mainItem
        |> List.head
        |> Maybe.map .amount
        |> Maybe.withDefault 0


getWeightAtPlant : Step -> Float
getWeightAtPlant step =
    -- At plant we don't really have a main item that we could use for the weight, so instead
    -- sum the weight of all the materials.
    step.material
        |> List.map .amount
        |> List.sum


listIngredients : Products -> List ProcessName
listIngredients products =
    products
        |> listIngredientProcesses
        |> List.map .name


listIngredientProcesses : Products -> List Process
listIngredientProcesses =
    -- List all the "material" entries from the "at plant" step
    listProcesses (.plant >> .material)



-- FIXME: should be useful in UI for selecting a processing
-- listProcessings : Products -> List ProcessName
-- listProcessings products =
--     products
--         |> listProcessingProcesses
--         |> List.map .name


listProcessingProcesses : Products -> List Process
listProcessingProcesses =
    -- List all the "processing" entries from the "at plant" step
    listProcesses (.plant >> .processing)


listProcesses : (Product -> Items) -> Products -> List Process
listProcesses getStepItems products =
    products
        |> AnyDict.values
        |> List.concatMap (getStepItems >> List.map .process)
        |> LE.uniqueBy (.name >> Process.nameToString)
        |> List.sortBy (.name >> Process.nameToString)


addMaterial : List Process -> ProcessName -> Product -> Result String Product
addMaterial processes processName ({ plant } as product) =
    Process.findByName processes processName
        |> Result.map
            (\process ->
                let
                    amount =
                        1.0

                    newItem =
                        { amount = amount
                        , comment = ""
                        , process = process
                        , mainItem = False
                        }

                    withAddedItem =
                        { plant
                            | material = newItem :: plant.material
                        }

                    originalWeight =
                        getWeightAtPlant plant
                in
                { product | plant = withAddedItem }
                    |> updateProductAmounts originalWeight
            )


updateMaterialAmount : Item -> Amount -> Product -> Product
updateMaterialAmount itemToUpdate amount ({ plant } as product) =
    let
        originalWeight =
            getWeightAtPlant plant
    in
    { product
        | plant =
            { plant
                | material =
                    plant.material
                        |> List.map
                            (\item ->
                                if item == itemToUpdate then
                                    { item | amount = amount }

                                else
                                    item
                            )
            }
    }
        |> updateProductAmounts originalWeight


removeMaterial : Item -> Product -> Product
removeMaterial itemToRemove ({ plant } as product) =
    let
        originalWeight =
            getWeightAtPlant plant
    in
    { product
        | plant =
            { plant
                | material = List.filter (\item -> item /= itemToRemove) plant.material
            }
    }
        |> updateProductAmounts originalWeight


getAmountRatio : Float -> Product -> Float
getAmountRatio originalWeight currentProduct =
    let
        updatedWeight =
            getWeightAtPlant currentProduct.plant
    in
    -- We need the new "ratio" between the original product and the updated one,
    -- to change the amount for all the other processes (but the plant materials).
    updatedWeight
        / originalWeight


updateProductAmounts : Float -> Product -> Product
updateProductAmounts originalWeight ({ consumer, supermarket, distribution, packaging, plant } as product) =
    let
        amountRatio =
            getAmountRatio originalWeight product
    in
    { product
        | consumer = updateStepAmounts amountRatio consumer
        , supermarket = updateStepAmounts amountRatio supermarket
        , distribution = updateStepAmounts amountRatio distribution
        , packaging = updateStepAmounts amountRatio packaging
        , plant = updatePlantAmounts amountRatio plant
    }


updateStepAmounts : Float -> Step -> Step
updateStepAmounts amountRatio ({ material, transport, wasteTreatment, energy, processing } as step) =
    { step
        | material = updateAffectationAmounts amountRatio material
        , transport = updateAffectationAmounts amountRatio transport
        , wasteTreatment = updateAffectationAmounts amountRatio wasteTreatment
        , energy = updateAffectationAmounts amountRatio energy
        , processing = updateAffectationAmounts amountRatio processing
    }


{-| updatePlantAmounts is specific to the plant where we don't want to automatically update the materials
as they are customised by the user.
-}
updatePlantAmounts : Float -> Step -> Step
updatePlantAmounts amountRatio ({ transport, wasteTreatment, energy, processing } as step) =
    { step
      -- We DON'T update the material amounts, they are customised by the user
        | transport = updateAffectationAmounts amountRatio transport
        , wasteTreatment = updateAffectationAmounts amountRatio wasteTreatment
        , energy = updateAffectationAmounts amountRatio energy
        , processing = updateAffectationAmounts amountRatio processing
    }


updateAffectationAmounts : Float -> Items -> Items
updateAffectationAmounts amountRatio items =
    items
        |> List.map
            (\item ->
                { item | amount = item.amount * amountRatio }
            )


updatePlantTransport : Product -> List Process -> List Impact.Definition -> Country.Code -> Distances -> Product -> Product
updatePlantTransport originalProduct processes impactDefinitions countryCode distances ({ plant } as product) =
    let
        defaultTransport =
            originalProduct.plant.transport

        originalPlantWeight =
            getWeightAtPlant originalProduct.plant

        plantWeight =
            getWeightAtPlant product.plant

        amountRatio =
            plantWeight / originalPlantWeight

        -- If we changed the recipe, we don't want the default transports, with want the default transports
        -- with the updated amounts corresponding to the new recipe weight
        defaultTransportWithAjustedWeight =
            updateAffectationAmounts amountRatio defaultTransport

        impacts =
            Impact.impactsFromDefinitons impactDefinitions

        transport =
            distances
                |> Transport.getTransportBetween impacts countryCode defaultCountry

        transportWithRatio =
            transport
                -- We want the transport ratio for the plane to be 0 for food (for now)
                -- Cf https://fabrique-numerique.gitbook.io/ecobalyse/textile/transport#part-du-transport-aerien
                |> Formula.transportRatio (Unit.Ratio 0)

        toTonKm km =
            Length.inKilometers km * plantWeight / 1000

        transports =
            Process.loadWellKnown processes
                |> Result.map
                    (\wellKnown ->
                        [ ( wellKnown.lorryTransport, transportWithRatio.road )
                        , ( wellKnown.boatTransport, transportWithRatio.sea )
                        , ( wellKnown.planeTransport, transportWithRatio.air )
                        ]
                            |> List.map
                                (\( process, distance ) ->
                                    { amount = toTonKm distance
                                    , comment = ""
                                    , process = process
                                    , mainItem = False
                                    }
                                )
                    )
                |> Result.withDefault []
    in
    { product
        | plant =
            { plant
                | transport =
                    if countryCode == defaultCountry then
                        defaultTransportWithAjustedWeight

                    else
                        transports
            }
    }
