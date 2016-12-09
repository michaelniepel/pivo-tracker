port module Main exposing (..)

import Html exposing (Html, div, h1, text, input, form, button, thead, ul, li, footer, table, tr, th, td, tbody, tfoot, fieldset)
import Html.Attributes exposing (class, placeholder, type_, value, colspan)
import Html.Events exposing (onInput, onSubmit, onClick)


-- MODEL


type alias Model =
    { programmers : List Programmer
    , nameInput : String
    , paidBeers : Int
    , totalBeers : Int
    }


type alias Programmer =
    { id : Int
    , name : String
    , beers : Int
    , beersOnBill : Int
    , paid : Int
    }


initModel : ( Model, Cmd Msg )
initModel =
    ( { programmers = []
      , nameInput = ""
      , paidBeers = 0
      , totalBeers = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = DrinkBeer Int
    | DrinkBeerFromKey Int
    | AddProgrammer
    | Input String
    | PayBeersAndStay Int
    | PayBeersAndGoHome Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input nameInput ->
            Debug.log "Input updated model"
                ( { model | nameInput = nameInput }, Cmd.none )

        DrinkBeer who ->
            ( drinkBeer model who, Cmd.none )

        AddProgrammer ->
            if (String.isEmpty model.nameInput) then
                ( model, Cmd.none )
            else
                ( add model, Cmd.none )

        DrinkBeerFromKey who ->
            update (DrinkBeer who) model

        PayBeersAndStay who ->
            ( payBeersAndStay model who, Cmd.none )

        PayBeersAndGoHome who ->
            ( payBeersAndGoHome model who, Cmd.none )


payBeersAndStay : Model -> Int -> Model
payBeersAndStay model who =
    payBeers model who


payBeersAndGoHome : Model -> Int -> Model
payBeersAndGoHome model who =
    let
        newModel =
            payBeers model who
    in
        goHome newModel who


goHome : Model -> Int -> Model
goHome model who =
    let
        newProgrammers =
            List.filter
                (\programmer ->
                    programmer.id /= who
                )
                model.programmers
    in
        { model | programmers = newProgrammers }


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
            Programmer (List.length model.programmers + 1) model.nameInput 0 0 0

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
            , totalBeers = model.totalBeers + 1
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
            [ th [ class "w10" ] [ text "Id" ]
            , th [ colspan 2 ] [ text "Meno" ]
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
        [ td [ class "w10" ] [ text (toString programmer.id) ]
        , td [] [ text programmer.name ]
        , td [ class "w40" ]
            [ button [ type_ "button", onClick (DrinkBeer programmer.id), class "pure-button button-secondary" ] [ text "Pi pivo" ]
            , button [ type_ "button", onClick (PayBeersAndStay programmer.id), class "pure-button button-warning" ] [ text "Zaplat" ]
            , button [ type_ "button", onClick (PayBeersAndGoHome programmer.id), class "pure-button button-error" ] [ text "Odchod" ]
            ]
        , td [] [ text ((toString programmer.beersOnBill) ++ " / " ++ (toString programmer.beers) ++ " / " ++ (toString programmer.paid)) ]
        ]


totalBeers : Model -> Html a
totalBeers model =
    tfoot []
        [ tr []
            [ td [ colspan 3, class "total-label" ] [ text "Celkovo piv" ]
            , td [] [ text (toString model.totalBeers) ]
            ]
        , tr []
            [ td [ colspan 3, class "total-label" ] [ text "Zaplatene" ]
            , td [] [ text (toString model.paidBeers) ]
            ]
        ]



-- SUBSCRIPTIONS


port piPivo : (Int -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    piPivo DrinkBeerFromKey


main : Program Never Model Msg
main =
    Html.program
        { init = initModel
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
