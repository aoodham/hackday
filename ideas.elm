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
model = initialModel


emptyModel : Model
emptyModel = Model
  (Idea "" "" "" "")
  []


initialModel : Model
initialModel = Model
  (Idea "" "" "" "")
  [ Idea "Idea title" "This is a description of the idea" "Douglas" "Nope"
  , Idea "Hackday ideas board" "Whose idea was this, it's great" "Who" "YEAH"
  ]


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
  div [style containerStyle]
    [ viewNewIdea model.new
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
    , button [ onClick Add ] [ text "Submit" ]
    ]

containerStyle : List (String, String)
containerStyle =
  [ ("display", "flex")
  , ("flex", "1")
  , ("flexDirection", "column")
  , ("justifyContent", "center")
  , ("alignItems", "center")
  ]

cardStyle : List (String, String)
cardStyle =
  [ ("flexDirection", "row")
  , ("justifyContent", "space-between")
  , ("alignItems", "center")
  , ("padding", "10px")
  , ("margin", "5px")
  , ("border", "1px solid")
  ]

titleStyle: List (String, String)
titleStyle =
  [ ("fontSize", "30px")
  , ("fontWeight", "bold")
  , ("margin", "2px")
  , ("border", "1px dotted")
  ]
