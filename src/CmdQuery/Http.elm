module CmdQuery.Http exposing
    ( Query, State, Msg
    , get, Expect, expectJson
    , Error
    , CachedResult
    )

{-| This module allows you to make Http request in queries.

The API closely follows the one from the `elm/http` package. The differences are basically:

  - Instead of returning `Cmds`, when making requests, we reutrn `Query`s.
  - The results of our queries are wrapped in a `Maybe`, which will be `Nothing`
    if the request has not completed.

Currently not every function from `elm/http` has an equivalent here. There are more to come,
and contributions in this area are very welcome!


# Shorthands

@docs Query, State, Msg


# Making requests

Each of these corresponds to the same thing from the `http` package.

@docs get, Expect, expectJson


# Errors

@docs Error


# Low level

@docs CachedResult

-}

import CSexpr.Encode as CE
import CmdQuery
import Http
import Json.Decode as JD
import Json.Encode as JE



{- `CmdQuery.Query`, specialized for http requests. -}


type alias Query a =
    CmdQuery.Query String CachedResult a


{-| `CmdQuery.State`, specialized for http requests.
-}
type alias State =
    CmdQuery.State String CachedResult


{-| `CmdQuery.Msg`, specialized for http requests.
-}
type alias Msg =
    CmdQuery.Msg String CachedResult


{-| Saved request results. You shouldn't have to do anything with this yourself.
-}
type CachedResult
    = CRJson (Result Http.Error JE.Value)


{-| Works just like `Http.Expect` from the `http` package. Note however that
it's a different type, so you can't mix the two.
-}
type Expect msg
    = ExpectJSON (Result Http.Error JE.Value -> msg)


{-| Make an HTTP GET request. Like `Http.get`.
-}
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


{-| `Http.Error`, re-exported for convienence
-}
type alias Error =
    Http.Error
