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
model = emptyModel


emptyModel : Model
emptyModel = Model
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
      emptyModel

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
  div [style cardStyle]
  [ div [style titleStyle] [text (idea.title)]
  , div [] [text (idea.description)]
  , div [] [text (idea.person)]
  , div [] [text (idea.status)]
  ]

viewNewIdea : Idea -> Html Msg
viewNewIdea idea =
   div []
    [ input [ placeholder "Title", value idea.title, onInput UpdateTitle ] [ text idea.title ]
    , input [ placeholder "Description", value idea.description, onInput UpdateDescription ] [ text idea.description ]
    , input [ placeholder "Person who proposed the idea", value idea.person, onInput UpdatePerson ] [ text idea.person ]
    , input [ placeholder "New", value idea.status, onInput UpdateStatus ] [ text idea.status ]
    ]

cardStyle : List (String, String)
cardStyle =
  [ ("display" , "flex")
  , ("flex", "1")
  , ("flexDirection", "row")
  , ("justifyContent", "space-between")
  , ("alignItems", "center")
  , ("margin", "100px")
  , ("border", "1px solid")
  ]

titleStyle: List (String, String)
titleStyle =
  [ ("fontSize", "30px")
  , ("fontWeight", "bold")
  , ("margin", "2px")
  , ("border", "1px dotted")
  ]
