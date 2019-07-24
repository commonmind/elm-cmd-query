module CmdQuery.Http exposing (CachedResult, Expect, Query, expectJson, get)

import CSexpr.Encode as CE
import CmdQuery
import Http
import Json.Decode as JD
import Json.Encode as JE


type alias Query a =
    CmdQuery.Query String CachedResult a


type CachedResult
    = CRJson (Result Http.Error JE.Value)


type Expect msg
    = ExpectJSON (Result Http.Error JE.Value -> msg)


get : { url : String, expect : Expect msg } -> Query (Maybe msg)
get { url, expect } =
    CmdQuery.get
        (CE.encodeString <|
            CE.list
                [ CE.s "CmdQuery.Http"
                , CE.list [ CE.s "method", CE.s "GET" ]
                , CE.list [ CE.s "url", CE.s url ]
                , CE.list [ CE.s "bodyType", bodyTypeKey expect ]
                ]
        )
        (Http.get
            { url = url
            , expect = convertExpect expect
            }
        )
        |> CmdQuery.map (Maybe.map <| handleResult expect)


bodyTypeKey : Expect msg -> CE.Encoder
bodyTypeKey (ExpectJSON _) =
    CE.s "json"


handleResult : Expect msg -> CachedResult -> msg
handleResult (ExpectJSON f) (CRJson result) =
    f result


convertExpect : Expect msg -> Http.Expect CachedResult
convertExpect expect =
    case expect of
        ExpectJSON _ ->
            Http.expectJson CRJson JD.value


expectJson : (Result Http.Error a -> msg) -> JD.Decoder a -> Expect msg
expectJson f dec =
    ExpectJSON
        (Result.andThen
            (JD.decodeValue dec
                >> Result.mapError (JD.errorToString >> Http.BadBody)
            )
            >> f
        )
