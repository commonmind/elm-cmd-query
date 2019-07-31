Easy generation of data based on the results of `Cmd`s.

Note: if you're viewing this README on GitHub, be aware that this is a
mirror of our [GitLab repo][2]; please open issues and pull requests
there.

# Introduction

It's commonly the case with single page applications that you want to
display some information to the user, but some of it needs to be fetched
from the server first. This means you have to keep track of what data
has been fetched and what hasn't, and issue requests as appropriate.
Especially when fetching all the data you want involves several Cmds,
the book-keeping involved can be quite cumbersome.

This library solves that. Rather than manually tracking outstanding
requests and updating available data, you write a `Query`, which feels a
lot like writing a `Json.Decoder`. When writing the query you get to
ignore all of the details of managing request state, and just say "I
need the data from this request". If it isn't available yet, you'll get
a `Nothing` and can render a loading spinner or something, otherwise you
have the data and can use it.

The library can examine your query and automatically figure out what
commands need to be run to get missing data.

# Example

```elm
import CmdQuery
import CmdQuery.Http as Http
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
        , expect = Http.expectJson D.int
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
            CmdQuery.map (\data -> (name, data)) (playerScoreQuery name)
        )
    |> CmdQuery.combine
    |> CmdQuery.map Dict.fromList

-- MODEL

type alias Model = Model
    -- We store a single `State` value in our model, which is managed by
    -- the library.
    { queryState : Http.State
    }

type Msg
    -- Our message type wraps updates to the state.
    = UpdateState Http.Msg

init : (Model, Cmd Msg)
init =
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
view (Model {queryState}) =
    -- CmdQuery.value gets the result of the query, based on available data
    -- from the state.
    viewData (CmdQuery.value queryState allScoresQuery)

viewScore : Maybe (Result Http.Error Int) -> Html Msg
viewScore maybeScore =
    Nothing ->
        text "Still loading..."

    Err e ->
        text ("There was a problem: " ++ Debug.toString err)

    Ok score ->
        text (String.fromInt score)

viewData : Dict String (Maybe (Result Http.Error Int)) -> Html Msg
viewData dict =
    let data =
            Dict.toList dict
            |> List.map
                (\(name, score) ->
                    tr [] [ td [] [ text name ], td [] [ viewScore score ] ]
                )
    in table
        []
        (   tr [] [ th [] [ text "Player" ], th [] [ text "Score" ] ]
            :: data
        )
```

# License

Copyright (C) 2019 CommonMind LLC

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

[1]: https://tools.ietf.org/html/rfc4180
[2]: https://gitlab.com/commonmind/elm-csv-encode
