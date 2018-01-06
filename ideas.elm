import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Idea =
  { id : Int
  , title : String
  , description: String
  , person: String
  , votes: Int
  }

type alias Model =
  { new: Idea
  , saved: List Idea
  }

emptyNewIdea n =
    Idea (getNewId n) "" "" "" 0

model : Model
model = initialModel


emptyModel : Model
emptyModel = Model
  (emptyNewIdea -1)
  []

initialModel : Model
initialModel = Model
  (Idea 2 "" "" "" 0)
  [ Idea 0 "Idea title" "This is a description of the idea" "Douglas" 0
  , Idea 1 "Hackday ideas board" "Whose idea was this, it's great" "Who" 100
  ]

-- Helpers

getNewId: Int -> Int
getNewId n = if n < 0 then 0 else n + 1

onVote: Int  -> Int -> Idea -> List Idea -> List Idea
onVote id vote new ideas =
      if new.id == id then
        {new | votes = new.votes + vote}::ideas
      else
        new::ideas

onDelete: Int  -> Idea -> List Idea -> List Idea
onDelete id new ideas =
      if new.id == id then
        ideas
      else
        new::ideas

-- UPDATE

type Msg = Add
          | Reset
          | UpdateTitle String
          | UpdateDescription String
          | UpdatePerson String
          | Vote Int Int
          | Delete Int

update : Msg -> Model -> Model
update msg {new, saved} =
  case msg of
    Add ->
      Model
        (emptyNewIdea new.id)
        (saved ++ [new])

    Reset ->
      emptyModel

    UpdateTitle title ->
      Model
        { new | title = title }
        saved

    UpdateDescription description ->
      Model
        { new | description = description }
        saved

    UpdatePerson person ->
      Model
        { new | person = person }
        saved

    Vote id v ->
      Model
        new
        (List.foldr (onVote id v) [] saved)

    Delete id ->
      Model
        new
        (List.foldr (onDelete id) [] saved)

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
  [ div [style titleStyle] [text (toString idea.id ++ " " ++ idea.title)]
  , div [] [text (idea.description)]
  , div [] [text (idea.person)]
  , viewIdeaStatus idea
  , div [] [text (toString idea.votes)]
  , button [ onClick (Vote idea.id 1) ] [ text "Upvote" ]
  , button [ onClick (Vote idea.id -1) ] [ text "Downvote" ]
  , button [ onClick (Delete idea.id) ] [ text "Delete" ]
  ]

viewNewIdea : Idea -> Html Msg
viewNewIdea idea =
   div []
    [ input [ placeholder "Title", value idea.title, onInput UpdateTitle ] [ text idea.title ]
    , input [ placeholder "Description", value idea.description, onInput UpdateDescription ] [ text idea.description ]
    , input [ placeholder "Person who proposed the idea", value idea.person, onInput UpdatePerson ] [ text idea.person ]
    , button [ onClick Add ] [ text "Submit" ]
    ]

viewIdeaStatus : Idea -> Html msg
viewIdeaStatus idea =
  let
    (color, status) =
      if idea.votes < 0 then
        ("red", "Nope")
      else if idea.votes < 5 then
        ("orange", "Maybe")
      else
        ("green", "Let's do this!")
  in
    div [ style [("color", color)] ] [ text status ]

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
