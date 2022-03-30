module Player exposing (..)

import Browser.Navigation exposing (forward)
import List.Extra as ListX
import Warrior exposing (Action(..), Warrior, position)
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction as Dir
import Warrior.History exposing (History, previousActions, previousStates)
import Warrior.Map exposing (Map, look)
import Warrior.Map.Tile exposing (Tile(..), canMoveOnto)


type ScoredDirection
    = ScoredDirection Dir.Direction Int


flipDirection : Dir.Direction -> Dir.Direction
flipDirection dir =
    case dir of
        Dir.Down ->
            Dir.Up

        Dir.Up ->
            Dir.Down

        Dir.Left ->
            Dir.Right

        Dir.Right ->
            Dir.Left


rateTile : (Coordinate -> Bool) -> ( Coordinate, Tile ) -> Int
rateTile visited ( coordinate, tile ) =
    case tile of
        Wall ->
            0

        Empty ->
            if visited coordinate then
                -1

            else
                2

        SpawnPoint ->
            -2

        Exit ->
            5

        Warrior _ ->
            -1

        Item _ ->
            2


nonEmptyList : List a -> Maybe (List a)
nonEmptyList list =
    case list of
        [] ->
            Nothing

        _ ->
            Just list


rateByLastDirection : Warrior -> History -> Dir.Direction -> Int
rateByLastDirection warrior history direction =
    let
        mapDirection : Action -> Maybe Dir.Direction
        mapDirection action =
            case action of
                Move dir ->
                    Just dir

                _ ->
                    Nothing

        lastDirection =
            previousActions warrior history
                |> ListX.findMap mapDirection

        backRating =
            Maybe.map flipDirection lastDirection
                |> Maybe.map
                    (\back ->
                        if direction == back then
                            -5

                        else
                            0
                    )
                |> Maybe.withDefault 0

        forwardRating =
            lastDirection
                |> Maybe.map
                    (\forward ->
                        if direction == forward then
                            3

                        else
                            0
                    )
                |> Maybe.withDefault 0
    in
    backRating + forwardRating


scoredDirections : Warrior -> Map -> History -> List ScoredDirection
scoredDirections warrior map history =
    let
        visited coordinate =
            List.member coordinate <|
                (previousStates warrior history
                    |> List.map (\( war, _ ) -> position war)
                )

        initialDirectionRating = rateByLastDirection warrior history

        rateDirectionPath : ( Dir.Direction, List ( Coordinate, Tile ) ) -> ScoredDirection
        rateDirectionPath (dir, path) =
            path
                |> List.map (rateTile visited)
                |> List.foldl (+) (initialDirectionRating dir)
                |> ScoredDirection dir

        directionPath dir =
            look dir warrior map
                |> ListX.takeWhile (\( _, tile ) -> canMoveOnto tile)
                |> nonEmptyList
                |> Maybe.map (\tiles -> ( dir, tiles ))
    in
    Dir.all
        |> List.filterMap directionPath
        |> List.map rateDirectionPath



{-
   TODO:
-}

takeTurn : Warrior -> Map -> History -> Warrior.Action
takeTurn warrior map history =
    scoredDirections warrior map history
        |> ListX.maximumBy (\(ScoredDirection _ score) -> score)
        |> Maybe.map (\(ScoredDirection dir _) -> Warrior.Move dir)
        |> Maybe.withDefault Warrior.Wait
