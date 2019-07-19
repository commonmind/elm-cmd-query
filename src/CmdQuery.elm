module CmdQuery exposing
    ( get
    , fetchNeeded, value
    , flatten, return, andThen, andMap
    , map, map2, map3, map4, map5
    , Query, State
    )

{-|

@docs Query State Msg

@docs get

@docs fetchNeeded, value

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


type Query comparable cmd v a
    = Flatten (Query comparable cmd v (Query comparable cmd v a))
    | Get comparable cmd (Maybe v -> Query comparable cmd v a)
    | Return a


type State comparable v
    = State
        { data : Dict comparable (Maybe v)
        }


type Msg comparable v
    = StateResult comparable v


flatten : Query comparable cmd v (Query comparable cmd v a) -> Query comparable cmd v a
flatten =
    Flatten


get : comparable -> cmd -> Query comparable cmd v (Maybe v)
get key cmd =
    Get key cmd return


return : a -> Query comparable cmd v a
return =
    Return


fetchNeeded :
    (cmd -> Cmd v)
    -> State comparable v
    -> Query comparable cmd v a
    -> ( State comparable v, Cmd (Msg comparable v) )
fetchNeeded toCmd (State s) query =
    case query of
        Return _ ->
            ( State s, Cmd.none )

        Get key cmd f ->
            let
                next s2 =
                    let
                        q2 =
                            f Nothing

                        ( s3, cmd2 ) =
                            fetchNeeded toCmd s2 q2
                    in
                    ( s3
                    , Cmd.batch
                        [ Cmd.map (StateResult key) (toCmd cmd)
                        , cmd2
                        ]
                    )
            in
            case Dict.get key s.data of
                Nothing ->
                    next <| State { s | data = Dict.insert key Nothing s.data }

                Just val ->
                    next <| State s

        Flatten q2 ->
            fetchNeededAux toCmd (State s) q2


value : State comparable v -> Query comparable cmd v a -> a
value (State s) query =
    case query of
        Return x ->
            x

        Get key _ f ->
            Dict.get key s.data
                |> Maybe.Extra.join
                |> f
                |> value (State s)

        Flatten query2 ->
            valueAux (State s) query2
                |> valueAux (State s)


map : (a -> b) -> Query comparable cmd v a -> Query comparable cmd v b
map f query =
    case query of
        Flatten inner ->
            Flatten <| mapAux (mapAux f) inner

        Get key cmd oldF ->
            Get key cmd (oldF >> mapAux f)

        Return x ->
            Return (f x)



-- Boilerplate combinators. These make no use of the logic particular to CmdQuery,
-- they're just generic formulae that work for any type supporting flatten, map,
-- and return -- unforunately elm can't abstract these out though.


andThen : (a -> Query comparable cmd v b) -> Query comparable cmd v a -> Query comparable cmd v b
andThen f x =
    flatten (map f x)


andMap : Query comparable cmd v a -> Query comparable cmd v (a -> b) -> Query comparable cmd v b
andMap x f =
    -- TODO: in principle, it should be possible to launch the commands for f and x
    -- independently, but the generic formulation based on andThen doesn't allow for this,
    -- because `fetchNeeded` can't see through it. It would be nice to update this so that
    -- it allows both commands to run concurrently.
    x |> andThen (\x2 -> f |> andThen (\f2 -> return (f2 x2)))


map2 :
    (a -> b -> c)
    -> Query comparable cmd v a
    -> Query comparable cmd v b
    -> Query comparable cmd v c
map2 fn a b =
    map fn a |> andMap b


map3 :
    (a -> b -> c -> d)
    -> Query comparable cmd v a
    -> Query comparable cmd v b
    -> Query comparable cmd v c
    -> Query comparable cmd v d
map3 fn a b c =
    map2 fn a b |> andMap c


map4 :
    (a -> b -> c -> d -> e)
    -> Query comparable cmd v a
    -> Query comparable cmd v b
    -> Query comparable cmd v c
    -> Query comparable cmd v d
    -> Query comparable cmd v e
map4 fn a b c d =
    map3 fn a b c |> andMap d


map5 :
    (a -> b -> c -> d -> e -> f)
    -> Query comparable cmd v a
    -> Query comparable cmd v b
    -> Query comparable cmd v c
    -> Query comparable cmd v d
    -> Query comparable cmd v e
    -> Query comparable cmd v f
map5 fn a b c d e =
    map4 fn a b c d |> andMap e


combine : List (Query comparable cmd v a) -> Query comparable cmd v (List a)
combine qs =
    case qs of
        [] ->
            return []

        x :: xs ->
            map2 (::) x (combine xs)



-- Helpers to work around a quirk in Elm's type inference.
--
-- namely, each function `fooAux` is exactly the same as `foo`, but we need the
-- indirection becuase the functions call themselves recursivley, where the recursive
-- call has a different type than the original. As a toy example, this will not
-- compile:
--
-- ```
-- type Wrap a = Raw a | Wrapped (Wrap (Wrap a))
--
-- unwrap : Wrap a -> a
-- unwrap arg =
--     case arg of
--         Raw x ->
--             x
--
--         Wrapped next ->
--             unwrap (unwrap next)
--
-- use = unwrap (Wrapped (Wrapped (Raw Int)))
-- ```
--
-- The reason is that the original call to `unwrap` (from `use)` has type
-- `Wrapped Int -> Int`, but outer recursive call must have type
-- `Wrapped (Wrapped Int) -> Wrapped Int`. Elm has lost track of the fact
-- that the type parameter can be *anything*, and so insists that it must
-- be the same in both calls.
--
-- Calling the *Aux functions instead seems to do the trick.


mapAux : (a -> b) -> Query comparable cmd v a -> Query comparable cmd v b
mapAux =
    map


valueAux : State comparable v -> Query comparable cmd v a -> a
valueAux =
    value


fetchNeededAux :
    (cmd -> Cmd v)
    -> State comparable v
    -> Query comparable cmd v a
    -> ( State comparable v, Cmd (Msg comparable v) )
fetchNeededAux =
    fetchNeeded
