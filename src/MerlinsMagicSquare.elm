module MerlinsMagicSquare exposing (..)

import Browser
import Debug
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Events exposing (onClick)
import Process
import Random
import Set exposing (Set)
import Html exposing (h1, h2)



-- MAIN


main =
    Browser.element { init = initState, update = update, view = view, subscriptions = \_ -> Sub.none }




type GameState
    = Initializing
    | Playing
    | Won

-- MODEL
-- The positions are from 1 to 9
-- A position is in the set if it is on, otherwise it is off
type alias MagicSquare =
    Set Int


type alias AppState =
    { gameState : GameState
    , magicSquare : MagicSquare
    , showHints : Bool
    , winningMoves : Maybe (List Int)
    }


initState : () -> ( AppState, Cmd Msg )
initState _ =
    ( { gameState = Initializing, magicSquare = Set.empty, showHints = False, winningMoves = Nothing }, Random.generate NewMagicSquare magicSquareGenerator )


magicSquareGenerator : Random.Generator MagicSquare
magicSquareGenerator =
    Random.list 4 (Random.int 1 9) |> Random.map Set.fromList


goalState : MagicSquare
goalState =
    Set.fromList (List.range 1 9) |> Set.remove 5



-- UPDATE
-- Types of messages that can be sent to the update function to update the model
type Msg
    = NewMagicSquare MagicSquare
    | Select Int
    | ToggleHints
    | RestartGame


update : Msg -> AppState -> ( AppState, Cmd Msg )
update msg state =
    case msg of
        RestartGame ->
            initState ()

        ToggleHints ->
            ( { state | showHints = not state.showHints }, Cmd.none )

        NewMagicSquare magicSquare ->
            ( { state | gameState = Playing, magicSquare = magicSquare }, Cmd.none )

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
                        ( { state | gameState = Won, magicSquare = nextSquare }, Cmd.none )

                    else
                        ( { state | gameState = Playing, magicSquare = nextSquare }, Cmd.none )

                Won ->
                    ( state, Cmd.none )


-- The cells that need to be changed when a cell is clicked
changeList : Int -> List Int
changeList pos =
    case pos of
        1 -> [ 1, 2, 4, 5 ]
        2 -> [ 1, 2, 3 ]
        3 -> [ 2, 3, 5, 6 ]
        4 -> [ 1, 4, 7 ]
        5 -> [ 2, 4, 5, 6, 8 ]
        6 -> [ 3, 6, 9 ]
        7 -> [ 4, 5, 7, 8 ]
        8 -> [ 7, 8, 9 ]
        9 -> [ 5, 6, 8, 9 ]
        _ -> []


-- toggle all cells in the list in a magic square
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
        [ h1 [] [ text "Merlin's Magic Square"]
        , div []
            [ text "The goal is to turn all the cells on (X), except the middle one (O)."
            , text "You can only turn on a cell and its neighbors. \n"
            ]
        , if appState.gameState == Won then
            h2 [] [text "You won!"]
          else
            text "Click on a cell to toggle it and its neighbors"
        , table []
            ([ 1, 2, 3 ] |> List.map viewRow)
        , if appState.showHints then
            div []
                [ text "The winning moves are: "
                , text << Debug.toString <| findWinningMoves appState.magicSquare
                ]

          else
            div []
                [ text "Number of moves this game can be won: "
                , text << String.fromInt <| List.length <| findWinningMoves appState.magicSquare
                ]
        , button [ onClick ToggleHints ]
            [ text "Click here to toggle hints"
            ]
        , button [ onClick RestartGame ]
            [ text "Click here to start a new game"
            ]
        ]



------ AI ------
-- We use breadth first search to find the winning moves
-- Since there are only 2^9 possible states, this is fast enough


hash : MagicSquare -> String
hash model =
    Set.toList model |> List.map String.fromInt |> String.join ""


findWinningMoves : MagicSquare -> List Int
findWinningMoves model =
    List.reverse <| searchStep Set.empty [ { path = [], state = model } ]



-- apperently we can't use a set of MagicSquare as a key in a set
-- so we use a hash instead. I'm very disappointed by this :(
type alias MagicSquareHash =
    String



-- the frontier contains the nodes we have not visited yet and nothing we have visited
searchStep : Set MagicSquareHash -> List { path : List Int, state : MagicSquare } -> List Int
searchStep visited frontier =
    let
        goalNodes =
            List.filter (\node -> node.state == goalState) frontier
    in
    case List.head goalNodes of
        Just node ->
            node.path

        -- We found a winning move
        Nothing ->
            let
                -- Expand frontier nodes
                newFrontier : List { path : List Int, state : MagicSquare }
                newFrontier =
                    List.concatMap (\node -> expand node.state |> List.map (\( x, y ) -> { path = x :: node.path, state = y })) frontier

                -- Insert frontier nodes into visited
                newVisited : Set MagicSquareHash
                newVisited =
                    List.foldl (\node vs -> Set.insert (hash node.state) vs) visited frontier

                -- Remove visited nodes from frontier
                newFrontierUnseen : List { path : List Int, state : MagicSquare }
                newFrontierUnseen =
                    List.filter (\node -> not (Set.member (hash node.state) newVisited)) newFrontier
            in
            searchStep newVisited newFrontierUnseen


expand : MagicSquare -> List ( Int, MagicSquare )
expand model =
    List.map (\x -> ( x, toggle model (changeList x) )) (List.range 1 9)
