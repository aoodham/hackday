import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Idea =
  { title : String
  , description: String
  , person: String
  , status: String
  }

type alias Model =
  { new: Idea
  , saved: List Idea
  }

model : Model
model = Model
  (Idea "" "" "" "")
  []


-- UPDATE

type Msg = Add
          | Reset
          | UpdateTitle String
          | UpdateDescription String
          | UpdateStatus String
          | UpdatePerson String

update : Msg -> Model -> Model
update msg {new, saved} =
  case msg of
    Add ->
      Model
        (Idea "" "" "" "")
        (saved ++ [new])

    Reset ->
      Model
        (Idea "" "" "" "")
        []

    UpdateTitle title ->
      Model
        {new | title = title}
        saved

    UpdateDescription description ->
      Model
        { new | description = description}
        saved

    UpdatePerson person ->
      Model
        { new | person = person}
        saved

    UpdateStatus status ->
      Model
        { new | status = status}
        saved


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewNewIdea model.new
    , button [ onClick Add ] [ text "+" ]
    , div [] (List.map makeDiv model.saved)
    , button [ onClick Reset ] [ text "reset" ]
    ]

makeDiv: Idea -> Html Msg
makeDiv idea =
  div [style [("color", "green")]] [text (idea.title ++ " " ++ idea.description ++ " " ++ idea.person ++ " " ++ idea.status)]

viewNewIdea : Idea -> Html Msg
viewNewIdea idea =
   div []
    [ input [ placeholder "Title", value idea.title, onInput UpdateTitle ] [ text idea.title ]
    , input [ placeholder "Description", value idea.description, onInput UpdateDescription ] [ text idea.description ]
    , input [ placeholder "Person who proposed the idea", value idea.person, onInput UpdatePerson ] [ text idea.person ]
    , input [ placeholder "New", value idea.status, onInput UpdateStatus ] [ text idea.status ]
    ]
