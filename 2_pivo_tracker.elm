module Main exposing (..)

import Html exposing (Html, div, h1, text, input, form, button, header, ul, li, footer)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit, onClick)


-- MODEL


type alias Model =
    { programmers : List Programmer
    , nameInput : String
    }


type alias Programmer =
    { id : Int
    , name : String
    , beers : Int
    }


initModel : Model
initModel =
    { programmers = []
    , nameInput = ""
    }



-- UPDATE


type Msg
    = DrinkBeer Int
    | AddProgrammer
    | Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input nameInput ->
            Debug.log "Input updated model"
                { model | nameInput = nameInput }

        DrinkBeer who ->
            drinkBeer model who

        AddProgrammer ->
            if (String.isEmpty model.nameInput) then
                model
            else
                add model


add : Model -> Model
add model =
    let
        programmer =
            Programmer (List.length model.programmers) model.nameInput 0

        newProgrammers =
            programmer :: model.programmers
    in
        { model
            | programmers = newProgrammers
            , nameInput = ""
        }


drinkBeer : Model -> Int -> Model
drinkBeer model id =
    let
        newProgrammers =
            List.map
                (\programmer ->
                    if programmer.id == id then
                        { programmer | beers = programmer.beers + 1 }
                    else
                        programmer
                )
                model.programmers
    in
        { model
            | programmers = newProgrammers
        }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "pivo-tracker" ]
        [ h1 [] [ text "Pivo tracker" ]
        , programmers model
        , programmerForm model
        ]


programmerForm : Model -> Html Msg
programmerForm model =
    form [ onSubmit AddProgrammer ]
        [ input [ type_ "text", placeholder "Meno programatora", onInput Input, value model.nameInput ] []
        , button [ type_ "submit" ] [ text "Pridaj" ]
        ]


programmers : Model -> Html Msg
programmers model =
    div []
        [ programmersHeader
        , programmersList model
        , totalBeers model
        ]


programmersHeader : Html a
programmersHeader =
    header []
        [ div [] [ text "Meno" ]
        , div [] [ text "Piv" ]
        ]


programmersList : Model -> Html Msg
programmersList model =
    --ul []
    --    (List.map programmer model.programmers)
    -- sorted, partial aplication & currying
    model.programmers
        |> List.sortBy .name
        |> List.map programmer
        |> ul []


programmer : Programmer -> Html Msg
programmer programmer =
    li []
        [ div [] [ text programmer.name ]
        , button [ type_ "button", onClick (DrinkBeer programmer.id) ] [ text "Pi pivo" ]
        , div [] [ text (toString programmer.beers) ]
        ]


totalBeers : Model -> Html a
totalBeers model =
    let
        total =
            List.map .beers model.programmers
                |> List.sum
    in
        footer []
            [ div [] [ text "Celkovo" ]
            , div [] [ text (toString total) ]
            ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
