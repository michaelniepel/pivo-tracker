module Main exposing (..)

import Html exposing (Html, div, h1, text, input, form, button, thead, ul, li, footer, table, tr, th, td, tbody, tfoot, fieldset)
import Html.Attributes exposing (class, placeholder, type_, value, colspan)
import Html.Events exposing (onInput, onSubmit, onClick)


-- MODEL


type alias Model =
    { programmers : List Programmer
    , nameInput : String
    , paidBeers : Int
    }


type alias Programmer =
    { id : Int
    , name : String
    , beers : Int
    , beersOnBill : Int
    , paid : Int
    }


initModel : Model
initModel =
    { programmers = []
    , nameInput = ""
    , paidBeers = 0
    }



-- UPDATE


type Msg
    = DrinkBeer Int
    | AddProgrammer
    | Input String
    | PayBeersAndStay Int
    | PayBeersAndGoHome Int


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

        PayBeersAndStay who ->
            payBeersAndStay model who

        PayBeersAndGoHome who ->
            payBeersAndGoHome model who


payBeersAndStay : Model -> Int -> Model
payBeersAndStay model who =
    payBeers model who


payBeersAndGoHome : Model -> Int -> Model
payBeersAndGoHome model who =
    model


payBeers : Model -> Int -> Model
payBeers model who =
    let
        total =
            List.map .beersOnBill model.programmers
                |> List.sum

        newProgrammers =
            List.map
                (\programmer ->
                    if programmer.id == who then
                        { programmer | beersOnBill = 0, paid = programmer.paid + total }
                    else
                        { programmer | beersOnBill = 0 }
                )
                model.programmers
    in
        { model | programmers = newProgrammers, paidBeers = model.paidBeers + total }


add : Model -> Model
add model =
    let
        programmer =
            Programmer (List.length model.programmers) model.nameInput 0 0 0

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
                        { programmer | beers = programmer.beers + 1, beersOnBill = programmer.beersOnBill + 1 }
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
    form [ onSubmit AddProgrammer, class "pure-form" ]
        [ fieldset []
            [ input [ type_ "text", placeholder "Meno programatora", onInput Input, value model.nameInput ] []
            , button [ type_ "submit", class "button-success pure-button" ] [ text "Pridaj" ]
            ]
        ]


programmers : Model -> Html Msg
programmers model =
    table [ class "pure-table pure-table-striped" ]
        [ programmersHeader
        , programmersList model
        , totalBeers model
        ]


programmersHeader : Html a
programmersHeader =
    thead []
        [ tr []
            [ th [ colspan 2 ] [ text "Meno" ]
            , th [ class "w15" ] [ text "Na ucte / Vypil / Zaplatil" ]
            ]
        ]


programmersList : Model -> Html Msg
programmersList model =
    --ul []
    --    (List.map programmer model.programmers)
    -- sorted, partial aplication & currying
    model.programmers
        |> List.sortBy .name
        |> List.map programmer
        |> tbody []


programmer : Programmer -> Html Msg
programmer programmer =
    tr []
        [ td [] [ text programmer.name ]
        , td [ class "w30" ]
            [ button [ type_ "button", onClick (DrinkBeer programmer.id), class "pure-button button-secondary" ] [ text "Pi pivo" ]
            , button [ type_ "button", onClick (PayBeersAndStay programmer.id), class "pure-button button-warning" ] [ text "Zaplat" ]
            ]
        , td [] [ text ((toString programmer.beersOnBill) ++ "/" ++ (toString programmer.beers) ++ "/" ++ (toString programmer.paid)) ]
        ]


totalBeers : Model -> Html a
totalBeers model =
    let
        total =
            List.map .beers model.programmers
                |> List.sum
    in
        tfoot []
            [ tr []
                [ td [ colspan 2, class "total-label" ] [ text "Celkovo piv" ]
                , td [] [ text (toString total) ]
                ]
            , tr []
                [ td [ colspan 2, class "total-label" ] [ text "Zaplatene" ]
                , td [] [ text (toString model.paidBeers) ]
                ]
            ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
