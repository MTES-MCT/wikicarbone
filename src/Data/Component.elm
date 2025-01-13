module Data.Component exposing
    ( Amount
    , Component
    , ComponentItem
    , DataContainer
    , ExpandedProcessItem
    , Id
    , ProcessItem
    , Quantity
    , Results
    , amountToFloat
    , available
    , componentItemToString
    , compute
    , computeComponentImpacts
    , decodeComponentItem
    , decodeListFromJsonString
    , emptyResults
    , encodeComponentItem
    , encodeId
    , expandComponentItems
    , expandProcessItems
    , extractImpacts
    , extractItems
    , extractMass
    , findById
    , idFromString
    , idToString
    , quantityFromInt
    , quantityToInt
    )

import Data.Common.DecodeUtils as DU
import Data.Impact as Impact exposing (Impacts)
import Data.Process as Process exposing (Process)
import Data.Uuid as Uuid exposing (Uuid)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Mass exposing (Mass)
import Quantity
import Result.Extra as RE


type Id
    = Id Uuid


{-| A Component is a named collection of processes and amounts of them
-}
type alias Component =
    { id : Id
    , name : String
    , processes : List ProcessItem
    }


{-| A compact representation of a component and a quantity of it
-}
type alias ComponentItem =
    { id : Id
    , quantity : Quantity
    }


{-| A Db-like interface holding components and processes
-}
type alias DataContainer db =
    { db
        | components : List Component
        , processes : List Process
    }


{-| A compact representation of an amount of material and an optional transformation of it
-}
type alias ProcessItem =
    { amount : Amount
    , material : Process.Id
    , transform : Maybe Process.Id
    }


type alias ExpandedProcessItem =
    { amount : Amount
    , material : Process
    , transform : Maybe Process
    }


type Amount
    = Amount Float


type Quantity
    = Quantity Int


{-| A nested data structure carrying the impacts and mass resulting from a computation
-}
type Results
    = Results
        { impacts : Impacts
        , items : List Results
        , mass : Mass
        }


{-| Add two results together
-}
addResults : Results -> Results -> Results
addResults (Results results) (Results acc) =
    Results
        { acc
            | impacts = Impact.sumImpacts [ results.impacts, acc.impacts ]
            , items = Results results :: acc.items
            , mass = Quantity.sum [ results.mass, acc.mass ]
        }


amountToFloat : Amount -> Float
amountToFloat (Amount float) =
    float


{-| List components which ids are not part of the provided list of ids
-}
available : List Id -> List Component -> List Component
available alreadyUsedIds =
    List.filter (\{ id } -> not <| List.member id alreadyUsedIds)
        >> List.sortBy .name


componentItemToString : DataContainer db -> ComponentItem -> Result String String
componentItemToString db { id, quantity } =
    db.components
        |> findById id
        |> Result.andThen
            (\component ->
                component.processes
                    |> RE.combineMap (processItemToString db.processes)
                    |> Result.map (String.join " | ")
                    |> Result.map
                        (\processesString ->
                            String.fromInt (quantityToInt quantity)
                                ++ " "
                                ++ component.name
                                ++ " [ "
                                ++ processesString
                                ++ " ]"
                        )
            )


{-| Computes impacts from a list of available components, processes and specified component items
-}
compute : DataContainer db -> List ComponentItem -> Result String Results
compute db =
    List.map (computeComponentItemResults db)
        >> RE.combine
        >> Result.map (List.foldr addResults emptyResults)


computeComponentImpacts : List Process -> Component -> Result String Results
computeComponentImpacts processes =
    .processes
        >> List.map (computeProcessItemResults processes)
        >> RE.combine
        >> Result.map (List.foldl addResults emptyResults)


computeComponentItemResults : DataContainer db -> ComponentItem -> Result String Results
computeComponentItemResults { components, processes } { id, quantity } =
    components
        |> findById id
        |> Result.andThen (.processes >> List.map (computeProcessItemResults processes) >> RE.combine)
        |> Result.map (List.foldr addResults emptyResults)
        |> Result.map
            (\(Results { impacts, mass, items }) ->
                Results
                    { impacts =
                        impacts
                            |> List.repeat (quantityToInt quantity)
                            |> Impact.sumImpacts
                    , items = items
                    , mass =
                        mass
                            |> List.repeat (quantityToInt quantity)
                            |> Quantity.sum
                    }
            )


computeProcessItemResults : List Process -> ProcessItem -> Result String Results
computeProcessItemResults processes { amount, material } =
    processes
        |> Process.findById material
        |> Result.map
            (\process ->
                let
                    impacts =
                        process.impacts
                            |> Impact.mapImpacts (\_ -> Quantity.multiplyBy (amountToFloat amount))

                    mass =
                        Mass.kilograms <|
                            if process.unit == "kg" then
                                amountToFloat amount

                            else
                                -- apply density
                                amountToFloat amount * process.density
                in
                Results
                    { impacts = impacts
                    , items = [ Results { impacts = impacts, items = [], mass = mass } ]
                    , mass = mass
                    }
            )


decodeListFromJsonString : String -> Result String (List Component)
decodeListFromJsonString =
    Decode.decodeString decodeList >> Result.mapError Decode.errorToString


{-| Take a list of component items and resolve them with actual components and processes
-}
expandComponentItems :
    DataContainer a
    -> List ComponentItem
    -> Result String (List ( Quantity, Component, List ExpandedProcessItem ))
expandComponentItems { components, processes } =
    List.map
        (\{ id, quantity } ->
            findById id components
                |> Result.andThen
                    (\component ->
                        component.processes
                            |> expandProcessItems processes
                            |> Result.map (\expandedItems -> ( quantity, component, expandedItems ))
                    )
        )
        >> RE.combine


{-| Take a list of process items and resolve them with actual processes
-}
expandProcessItems : List Process -> List ProcessItem -> Result String (List ExpandedProcessItem)
expandProcessItems processes =
    RE.combineMap
        (\{ amount, material, transform } ->
            Ok (ExpandedProcessItem amount)
                |> RE.andMap (Process.findById material processes)
                |> RE.andMap
                    (case transform of
                        Just id ->
                            Process.findById id processes |> Result.map Just

                        Nothing ->
                            Ok Nothing
                    )
        )


decode : Decoder Component
decode =
    Decode.succeed Component
        |> Decode.required "id" (Decode.map Id Uuid.decoder)
        |> Decode.required "name" Decode.string
        |> Decode.required "processes" (Decode.list decodeProcessItem)


decodeList : Decoder (List Component)
decodeList =
    Decode.list decode


decodeComponentItem : Decoder ComponentItem
decodeComponentItem =
    Decode.succeed ComponentItem
        |> Decode.required "id" (Decode.map Id Uuid.decoder)
        |> Decode.required "quantity" (Decode.map Quantity Decode.int)


decodeProcessItem : Decoder ProcessItem
decodeProcessItem =
    Decode.succeed ProcessItem
        |> Decode.required "amount" (Decode.map Amount Decode.float)
        |> Decode.required "material" Process.decodeId
        |> DU.strictOptional "transform" Process.decodeId


encodeComponentItem : ComponentItem -> Encode.Value
encodeComponentItem componentItem =
    Encode.object
        [ ( "id", componentItem.id |> idToString |> Encode.string )
        , ( "quantity", componentItem.quantity |> quantityToInt |> Encode.int )
        ]


encodeId : Id -> Encode.Value
encodeId =
    idToString >> Encode.string


{-| Lookup a Component from a provided Id
-}
findById : Id -> List Component -> Result String Component
findById id =
    List.filter (.id >> (==) id)
        >> List.head
        >> Result.fromMaybe ("Aucun composant avec id=" ++ idToString id)


idFromString : String -> Maybe Id
idFromString str =
    Uuid.fromString str |> Maybe.map Id


idToString : Id -> String
idToString (Id uuid) =
    Uuid.toString uuid


processItemToString : List Process -> ProcessItem -> Result String String
processItemToString processes processItem =
    processes
        |> Process.findById processItem.material
        |> Result.map
            (\process ->
                String.fromFloat (amountToFloat processItem.amount)
                    ++ process.unit
                    ++ " "
                    ++ Process.getDisplayName process
            )


quantityFromInt : Int -> Quantity
quantityFromInt int =
    Quantity int


quantityToInt : Quantity -> Int
quantityToInt (Quantity int) =
    int


emptyResults : Results
emptyResults =
    Results
        { impacts = Impact.empty
        , items = []
        , mass = Quantity.zero
        }


extractImpacts : Results -> Impacts
extractImpacts (Results { impacts }) =
    impacts


extractItems : Results -> List Results
extractItems (Results { items }) =
    items


extractMass : Results -> Mass
extractMass (Results { mass }) =
    mass
