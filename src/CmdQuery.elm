module CmdQuery exposing
    ( Query, State, Msg
    , get
    , fetchNeeded, value, update
    , flatten, return, andThen, andMap
    , map, map2, map3, map4, map5
    , combine, initialState
    )

{-|

@docs Query, State, Msg

@docs get

@docs fetchNeeded, value, update

@docs flatten, return, andThen, andMap

@docs map, map2, map3, map4, map5

-}

-- Copyright (C) 2019 CommonMind LLC
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

import Dict exposing (Dict)
import Maybe.Extra


type Query comparable v a
    = Query
        { fetcher : State comparable v -> ( State comparable v, Cmd (State comparable v -> State comparable v) )
        , value : State comparable v -> a
        }


type State comparable v
    = State
        { data : Dict comparable (Maybe v)
        }


type Msg comparable v
    = StateUpdate (State comparable v -> State comparable v)


return : a -> Query comparable v a
return x =
    Query
        { fetcher = \s -> ( s, Cmd.none )
        , value = \_ -> x
        }


map : (a -> b) -> Query comparable v a -> Query comparable v b
map f (Query q) =
    Query
        { fetcher = q.fetcher
        , value = q.value >> f
        }


andMap : Query comparable v a -> Query comparable v (a -> b) -> Query comparable v b
andMap (Query x) (Query f) =
    Query
        { fetcher =
            \s ->
                let
                    ( s2, cmd1 ) =
                        x.fetcher s

                    ( s3, cmd2 ) =
                        f.fetcher s2
                in
                ( s3, Cmd.batch [ cmd1, cmd2 ] )
        , value =
            \s ->
                f.value s (x.value s)
        }


flatten : Query comparable v (Query comparable v a) -> Query comparable v a
flatten (Query q) =
    Query
        { fetcher =
            \s ->
                let
                    ( s2, cmd1 ) =
                        q.fetcher s

                    (Query v) =
                        q.value s2

                    ( s3, cmd2 ) =
                        v.fetcher s2
                in
                ( s3, Cmd.batch [ cmd1, cmd2 ] )
        , value =
            \s ->
                let
                    (Query v) =
                        q.value s
                in
                v.value s
        }


initialState : State comparable v
initialState =
    State { data = Dict.empty }


update : Msg comparable v -> State comparable v -> State comparable v
update (StateUpdate f) =
    f


get : comparable -> Cmd v -> Query comparable v (Maybe v)
get key cmd =
    Query
        { fetcher =
            \(State s) ->
                case Dict.get key s.data of
                    Just _ ->
                        ( State s, Cmd.none )

                    Nothing ->
                        ( State { data = Dict.insert key Nothing s.data }
                        , cmd
                            |> Cmd.map
                                (\v (State s2) ->
                                    State { data = Dict.insert key (Just v) s2.data }
                                )
                        )
        , value =
            \(State s) ->
                Dict.get key s.data
                    |> Maybe.Extra.join
        }


fetchNeeded :
    State comparable v
    -> Query comparable v a
    -> ( State comparable v, Cmd (Msg comparable v) )
fetchNeeded s (Query q) =
    let
        ( s2, cmd ) =
            q.fetcher s
    in
    ( s2, Cmd.map StateUpdate cmd )


value : State comparable v -> Query comparable v a -> a
value s (Query q) =
    q.value s



-- Boilerplate combinators. These make no use of the logic particular to CmdQuery,
-- they're just generic formulae that work for any type supporting flatten, map,
-- and return -- unforunately elm can't abstract these out though.


andThen : (a -> Query comparable v b) -> Query comparable v a -> Query comparable v b
andThen f x =
    flatten (map f x)


map2 :
    (a -> b -> c)
    -> Query comparable v a
    -> Query comparable v b
    -> Query comparable v c
map2 fn a b =
    map fn a |> andMap b


map3 :
    (a -> b -> c -> d)
    -> Query comparable v a
    -> Query comparable v b
    -> Query comparable v c
    -> Query comparable v d
map3 fn a b c =
    map2 fn a b |> andMap c


map4 :
    (a -> b -> c -> d -> e)
    -> Query comparable v a
    -> Query comparable v b
    -> Query comparable v c
    -> Query comparable v d
    -> Query comparable v e
map4 fn a b c d =
    map3 fn a b c |> andMap d


map5 :
    (a -> b -> c -> d -> e -> f)
    -> Query comparable v a
    -> Query comparable v b
    -> Query comparable v c
    -> Query comparable v d
    -> Query comparable v e
    -> Query comparable v f
map5 fn a b c d e =
    map4 fn a b c d |> andMap e


combine : List (Query comparable v a) -> Query comparable v (List a)
combine =
    List.foldr (map2 (::)) (return [])
