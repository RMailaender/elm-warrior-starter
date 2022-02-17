module Player exposing (takeTurn)

import Html.Attributes exposing (dir)
import List.Extra as ListX
import Time exposing (Month(..))
import Warrior exposing (Warrior)
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction as Direction
import Warrior.History exposing (History, previousActions)
import Warrior.Map exposing (Map, look)
import Warrior.Map.Tile exposing (Tile, canMoveOnto, isExit)


type alias Position =
    { coordinate : Coordinate
    , tile : Tile
    }


toPosition : ( Coordinate, Tile ) -> Position
toPosition ( coordinate, tile ) =
    { coordinate = coordinate
    , tile = tile
    }


type alias Path =
    { dir : Direction.Direction
    , positions : List Position
    }


getPath : Warrior -> Map -> Direction.Direction -> Path
getPath warrior map dir =
    let
        positions =
            look dir warrior map
                |> ListX.takeWhile (\( _, tile ) -> canMoveOnto tile)
                |> List.map toPosition
    in
    { dir = dir
    , positions = positions
    }



-- actually a Path can have multiple weights like CurrentForward and Unexplored


type WeightedPath
    = CurrentForward Path
    | PathToExit Path
    | CurrentBackward Path
    | Unexplored Path
    | DeadEnd


compareWeightedPath : WeightedPath -> WeightedPath -> Order
compareWeightedPath a b =
    let
        weight : WeightedPath -> Int
        weight weightedPath =
            case weightedPath of
                PathToExit _ ->
                    9

                CurrentForward _ ->
                    8

                Unexplored _ ->
                    7

                CurrentBackward _ ->
                    6

                DeadEnd ->
                    0
    in
    compare (weight a) (weight b)


weightPath : List Warrior.Action -> Path -> WeightedPath
weightPath prevActions path =
    let
        isDeadEnd : Path -> Bool
        isDeadEnd { positions } =
            List.isEmpty positions

        -- visitedCoordinates =
        --     previousStates warrior history
        --         |> List.map (\( w, _ ) -> w)
        --         |> List.map position
        isMoveAction : Warrior.Action -> Bool
        isMoveAction action =
            case action of
                Warrior.Move _ ->
                    True

                _ ->
                    False

        isSameDirection : Direction.Direction -> Warrior.Action -> Bool
        isSameDirection pathDir action =
            case action of
                Warrior.Move dir ->
                    pathDir == dir

                _ ->
                    False

        lastMove =
            prevActions
                |> ListX.find isMoveAction

        isCurrentForward : Path -> Bool
        isCurrentForward { dir } =
            lastMove
                |> Maybe.map (isSameDirection dir)
                |> Maybe.withDefault False

        isCurrentBackward : Path -> Bool
        isCurrentBackward { dir } =
            let
                flippedDir =
                    flipDirection dir
            in
            lastMove
                |> Maybe.map (isSameDirection flippedDir)
                |> Maybe.withDefault False

        isPathToExit : Path -> Bool
        isPathToExit { positions } =
            positions
                |> List.any (.tile >> isExit)
    in
    if isPathToExit path then
        PathToExit path

    else if isDeadEnd path then
        DeadEnd

    else if isCurrentForward path then
        CurrentForward path

    else if isCurrentBackward path then
        CurrentBackward path

    else
        Unexplored path


findPaths : Warrior -> Map -> History -> List WeightedPath
findPaths warrior map history =
    Direction.all
        |> List.map (getPath warrior map)
        |> List.map (weightPath (previousActions warrior history))


flipDirection : Direction.Direction -> Direction.Direction
flipDirection dir =
    case dir of
        Direction.Down ->
            Direction.Up

        Direction.Up ->
            Direction.Down

        Direction.Left ->
            Direction.Right

        Direction.Right ->
            Direction.Left


takeTurn : Warrior -> Map -> History -> Warrior.Action
takeTurn warrior map history =
    let
        toAction : WeightedPath -> Warrior.Action
        toAction weightedPath =
            case weightedPath of
                PathToExit { dir } ->
                    Warrior.Move dir

                Unexplored { dir } ->
                    Warrior.Move dir

                CurrentForward { dir } ->
                    Warrior.Move dir

                CurrentBackward { dir } ->
                    Warrior.Move dir

                DeadEnd ->
                    Warrior.Wait
    in
    findPaths warrior map history
        |> ListX.maximumWith compareWeightedPath
        |> Maybe.map toAction
        |> Maybe.withDefault Warrior.Wait
