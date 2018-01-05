import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Idea =
  { name : String
  , description: String
  , person: String
  }

type alias Model =
  { new: Idea
  , saved: List Idea
  }

model : Model
model = Model
  (Idea "" "" "")
  []


-- UPDATE

type Msg = Add | Reset

update : Msg -> Model -> Model
update msg model =
  case msg of
    Add ->
      Model
        (Idea "" "" "")
        (model.saved ++ [Idea "a" "b" "c"])

    Reset ->
      Model
        (Idea "" "" "")
        []


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Add ] [ text "+" ]
    , div [] (List.map makeDiv model.saved)
    , button [ onClick Reset ] [ text "reset" ]
    ]

makeDiv: Idea -> Html Msg
makeDiv idea =
  div [] [text (idea.name ++ " " ++ idea.description ++ " " ++ idea.person)]
