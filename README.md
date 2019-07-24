Easy generation of data based on the results of `Cmd`s.

Note: if you're viewing this README on GitHub, be aware that this is a
mirror of our [GitLab repo][2]; please open issues and pull requests
there.

# Introduction

It's commonly the case with single page applications that you want to
display some information to the user, but some of it needs to be fetched
from the server first. This means you have to keep track of what data
has been fetched and what hasn't, and issue requests as appropriate.
Especially when there are many things that must be fetched individually,
this bookkeeping can be quite cumbersome.

This library solves that. Rather than manually tracking outstanding
requests and updating available data, you write a `Query`, which feels a
lot like writing a `Json.Decoder`. When writing the query you get to
ignore all of the details of managing request state, and just say "I
need the data from this request". If it isn't available yet, you'll get
a `Nothing` and can render a loading spinner or something, otherwise you
have the data and can use it.

The library can examine your query and automatically figure out what
commands need to be run to get missing data.

Example of writing a query:

```elm
-- TODO: finish this example
import CmdQuery
import CmdQuery.Http as Http
import Json.Decode as D

let players =
    [ "Alice"
    , "Bob"
    , "Charlie"
    ]

type alias GameData =
    { playerScores : Dict String Int
    , winner : Maybe String
    }

getGameData : Http.Query GameData
getGameData =
    getPlayerScores
        |> CmdQuery.map
            (\scores ->
                { playerScores = scores
                , winner =
                    Dict.toList scores
                        |> List.map (\(k, maybeV) -> maybe.map v
                    List.map (\k -> Dict.get k scores) players
                        |> Maybe.Extra.combine

getPlayerScores : Http.Query (Dict String Int)
getPlayerScores =
    players
        |> List.map
            (\player ->
                Http.get
                    { url = "/player-score/" ++ player
                    , expect =
                        Http.expectJson Result.toMaybe D.int
                    }
                |> CmdQuery.map (Maybe.withDefault Nothing)
                |> CmdQuery.map (\value -> (player, value))
            )
        |> CmdQuery.combine
        |> CmdQuery.map
            (List.filterMap
                (\(k, maybeV) -> Maybe.map (\v -> (k, v)) maybeV)
                >> Dict.fromList
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
