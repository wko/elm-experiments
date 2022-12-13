module TextInput exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, onInput)
import Html exposing (input)
import Html.Attributes exposing (..)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = 
  { content: String}


init : Model
init = { content = ""}



-- UPDATE


type Msg
  = Change String
  

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent}



-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Please enter some text", value model.content, onInput Change ] []
    , div [] [ text (String.reverse model.content) ]
    ]