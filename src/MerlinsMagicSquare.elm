module MerlinsMagicSquare exposing (..)

import Browser
import Debug
import Html exposing (Html, div, table, td, text, tr)
import Html.Events exposing (onClick)
import Random
import Set exposing (Set)



-- MAIN


main =
    Browser.element { init = initState, update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL
-- The positions are from 1 to 9
-- A position is in the set if it is on, otherwise it is off


type GameState
    = Initializing
    | Playing
    | Won


type alias MagicSquare =
    Set Int


type alias AppState =
    { gameState : GameState
    , magicSquare : MagicSquare
    }


initState : () -> ( AppState, Cmd Msg )
initState _ =
    ( { gameState = Initializing, magicSquare = Set.empty }, Random.generate NewMagicSquare magicSquareGenerator )


magicSquareGenerator : Random.Generator MagicSquare
magicSquareGenerator =
    Random.list 4 (Random.int 1 9) |> Random.map Set.fromList


goalState : MagicSquare
goalState =
    Set.fromList (List.range 1 9) |> Set.remove 5



-- UPDATE


type Msg
    = NewMagicSquare MagicSquare
    | Select Int


update : Msg -> AppState -> ( AppState, Cmd Msg )
update msg state =
    case msg of
        NewMagicSquare magicSquare ->
            ( { gameState = Playing, magicSquare = magicSquare }, Cmd.none )

        Select pos ->
            case state.gameState of
                Initializing ->
                    ( state, Cmd.none )

                Playing ->
                    let
                        nextSquare =
                            toggle state.magicSquare (changeList pos)
                    in
                    if nextSquare == goalState then
                        ( { gameState = Won, magicSquare = nextSquare }, Cmd.none )

                    else
                        ( { gameState = Playing, magicSquare = nextSquare }, Cmd.none )

                Won ->
                    ( state, Cmd.none )


changeList : Int -> List Int
changeList pos =
    case pos of
        1 ->
            [ 1, 2, 4, 5 ]

        2 ->
            [ 1, 2, 3 ]

        3 ->
            [ 2, 3, 5, 6 ]

        4 ->
            [ 1, 4, 7 ]

        5 ->
            [ 2, 4, 5, 6, 8 ]

        6 ->
            [ 3, 6, 9 ]

        7 ->
            [ 4, 5, 7, 8 ]

        8 ->
            [ 7, 8, 9 ]

        9 ->
            [ 5, 6, 8, 9 ]

        _ ->
            []


toggle : MagicSquare -> List Int -> MagicSquare
toggle model xs =
    List.foldl toggleCell model xs


toggleCell : Int -> MagicSquare -> MagicSquare
toggleCell x model =
    if Set.member x model then
        Set.remove x model

    else
        Set.insert x model



-- VIEW


view : AppState -> Html Msg
view appState =
    let
        viewCol row col =
            td [ onClick (Select (pos2idx row col)) ] (viewCell row col)

        viewRow row =
            tr [] (List.map (viewCol row) [ 1, 2, 3 ])

        viewCell row col =
            [ if Set.member (pos2idx row col) appState.magicSquare then
                text "X"

              else
                text "O"
            ]

        viewCell2 row col =
            [ text << String.fromInt <| pos2idx row col ]

        pos2idx row col =
            (row - 1) * 3 + col
    in
    div []
        [ if appState.gameState == Won then
            text "You won!"

          else
            text "Click on a cell to toggle it and its neighbors"
        , table []
            ([ 1, 2, 3 ] |> List.map viewRow)
        ]
