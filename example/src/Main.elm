module Main exposing (main)

import Browser
import CmdQuery
import CmdQuery.Http as Http
import Dict exposing (Dict)
import Html exposing (..)
import Json.Decode as D



-- QUERIES


players =
    [ "Alice"
    , "Bob"
    , "Charlie"
    ]


{-| Query the score for a particular player.
-}
playerScoreQuery : String -> Http.Query (Maybe (Result Http.Error Int))
playerScoreQuery name =
    -- The module CmdQuery.Http mirrors the elm/http package's API, so
    -- you can write http code in much the same way -- you just get
    -- a query instead of a Cmd.
    Http.get
        { url = "/scores/" ++ name
        , expect = Http.expectJson identity D.int
        }


{-| Query the scores of all the players.
-}
allScoresQuery : Http.Query (Dict String (Maybe (Result Http.Error Int)))
allScoresQuery =
    -- Complex queries can be composed using many familiar functions
    -- like `map`, `andThen`, and so on.
    players
        |> List.map
            (\name ->
                CmdQuery.map (\data -> ( name, data )) (playerScoreQuery name)
            )
        |> CmdQuery.combine
        |> CmdQuery.map Dict.fromList



-- MODEL


type Model
    = Model
        -- We store a single `State` value in our model, which is managed by
        -- the library.
        { queryState : Http.State
        }


type
    Msg
    -- Our message type wraps updates to the state.
    = UpdateState Http.Msg


init : {} -> ( Model, Cmd Msg )
init _ =
    let
        ( state, cmd ) =
            -- We use `fetchNeeded` to get a command that will fetch any missing
            -- data. The updated state knows that missing data has been requested,
            -- so it won't run the same commands twice.
            CmdQuery.fetchNeeded CmdQuery.initialState allScoresQuery
    in
    ( Model { queryState = state }
    , Cmd.map UpdateState cmd
    )



-- VIEW


view : Model -> Html Msg
view (Model { queryState }) =
    -- `CmdQuery.value` gets the result of the query, based on available data
    -- from the state.
    viewData (CmdQuery.value queryState allScoresQuery)


{-| `viewData` renders the current result of the query.
-}
viewData : Dict String (Maybe (Result Http.Error Int)) -> Html Msg
viewData dict =
    let
        data =
            Dict.toList dict
                |> List.map
                    (\( name, score ) ->
                        tr [] [ td [] [ text name ], td [] [ viewScore score ] ]
                    )
    in
    table
        []
        (tr [] [ th [] [ text "Player" ], th [] [ text "Score" ] ]
            :: data
        )


viewScore : Maybe (Result Http.Error Int) -> Html Msg
viewScore maybeScore =
    case maybeScore of
        Nothing ->
            text "Still loading..."

        Just (Err e) ->
            text ("There was a problem: " ++ Debug.toString e)

        Just (Ok score) ->
            text (String.fromInt score)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update (UpdateState stateMsg) (Model { queryState }) =
    let
        ( newState, cmd ) =
            CmdQuery.fetchNeeded
                (CmdQuery.update stateMsg queryState)
                allScoresQuery
    in
    ( Model { queryState = newState }
    , Cmd.map UpdateState cmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
