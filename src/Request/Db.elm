module Request.Db exposing (..)

import Data.Country as Country exposing (Country)
import Data.Db exposing (Db)
import Data.Material as Material exposing (Material)
import Data.Process as Process exposing (Process)
import Data.Product as Product exposing (Product)
import Data.Session exposing (Session)
import Data.Transport as Transport exposing (Distances)
import Json.Decode exposing (Decoder)
import RemoteData exposing (WebData)
import RemoteData.Http as Http exposing (defaultTaskConfig)
import Task exposing (Task)


taskConfig : Http.TaskConfig
taskConfig =
    -- drop ALL headers because Parcel's proxy messes with them
    -- see https://stackoverflow.com/a/47840149/330911
    { defaultTaskConfig | headers = [] }


getJson : Decoder a -> String -> Task () (WebData a)
getJson decoder file =
    Http.getTaskWithConfig taskConfig ("data/" ++ file) decoder


buildDb :
    WebData (List Process)
    -> WebData (List Country)
    -> WebData (List Material)
    -> WebData (List Product)
    -> WebData Distances
    -> WebData Db
buildDb processes countries materials products transports =
    RemoteData.succeed Db
        |> RemoteData.andMap processes
        |> RemoteData.andMap countries
        |> RemoteData.andMap materials
        |> RemoteData.andMap products
        |> RemoteData.andMap transports


getDb : Session -> Task () (WebData Db)
getDb _ =
    let
        -- see https://github.com/alex-tan/task-extra/blob/1.1.0/src/Task/Extra.elm#L579-L581
        andMap =
            Task.map2 (|>)
    in
    getJson Process.decodeList "processes.json"
        |> Task.andThen
            (\processesData ->
                case processesData of
                    RemoteData.Success processes ->
                        Task.succeed (buildDb processesData)
                            |> andMap (getJson (Country.decodeList processes) "countries.json")
                            |> andMap (getJson (Material.decodeList processes) "materials.json")
                            |> andMap (getJson (Product.decodeList processes) "products.json")
                            |> andMap (getJson Transport.decodeDistances "transports.json")

                    _ ->
                        Task.fail ()
            )


loadDb : Session -> (WebData Db -> msg) -> Cmd msg
loadDb session event =
    getDb session
        |> Task.attempt
            (\result ->
                case result of
                    Ok wd ->
                        event wd

                    Err _ ->
                        -- Note: this `Task () (WebData Db)` error situation can never happen
                        -- This is a limitation from the types returned by RemoteData.Http tasks
                        event RemoteData.NotAsked
            )
