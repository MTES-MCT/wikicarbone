module Request.Auth exposing
    ( AuthResponse(..)
    , Errors
    , login
    , logout
    , processes
    , register
    , user
    )

import Data.User as User exposing (User)
import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as Encode
import Static.Db as Db
import Static.Json exposing (RawJsonProcesses)


type alias Errors =
    Dict String String


type AuthResponse
    = SuccessResponse String
    | ErrorResponse String Errors


decodeAuthResponse : Decoder AuthResponse
decodeAuthResponse =
    Decode.field "success" Decode.bool
        |> Decode.andThen
            (\success ->
                if success then
                    Decode.field "msg" Decode.string
                        |> Decode.map SuccessResponse

                else
                    Decode.succeed ErrorResponse
                        |> JDP.required "msg" Decode.string
                        |> JDP.optional "errors"
                            (Decode.dict Decode.string
                                |> Decode.map (Dict.remove "email")
                            )
                            Dict.empty
            )


login : (Result Http.Error AuthResponse -> msg) -> String -> Cmd msg
login event email =
    Http.post
        { url = "/accounts/login/"
        , body = Http.jsonBody (Encode.object [ ( "email", Encode.string email ) ])
        , expect = Http.expectJson event decodeAuthResponse
        }


logout : msg -> Cmd msg
logout event =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = "/accounts/logout/"
        , body = Http.emptyBody
        , expect = Http.expectWhatever (always event)
        , timeout = Nothing
        , tracker = Nothing
        }


processes : (Result Http.Error RawJsonProcesses -> msg) -> String -> Cmd msg
processes event token =
    Http.request
        { method = "GET"
        , url = "processes/processes.json"
        , headers = [ Http.header "token" token ]
        , body = Http.emptyBody
        , expect = Http.expectJson event Db.decodeRawJsonProcesses
        , timeout = Nothing
        , tracker = Nothing
        }


register : (Result Http.Error AuthResponse -> msg) -> Encode.Value -> Cmd msg
register event userForm =
    Http.post
        { url = "/accounts/register/"
        , body = Http.jsonBody userForm
        , expect = Http.expectJson event decodeAuthResponse
        }


user : (Result Http.Error User -> msg) -> Cmd msg
user event =
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = "/accounts/profile/"
        , body = Http.emptyBody
        , expect = Http.expectJson event User.decode
        , timeout = Nothing
        , tracker = Nothing
        }
