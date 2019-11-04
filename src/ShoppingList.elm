module ShoppingList exposing (..)

import Domain.Item exposing (..)
import Http
import Html exposing (..)
import Html.Events exposing (onClick)
import Browser
import Array exposing (..)
import Bootstrap.Table as Table
import Bootstrap.Grid as Grid
import Bootstrap.CDN as CDN
import Json.Decode as Decode exposing (Decoder, Error(..), decodeString, list, int, string)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import RemoteData exposing (RemoteData, WebData)

type alias Model =
    { items : WebData (List Item) 
    }


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , div []
            [ button [ onClick SendHttpRequest ]
                [ text "Refresh data" ]
            , viewItemsOrError model
            ]
        ]


viewItemsOrError : Model -> Html Msg
viewItemsOrError model =
    case model.items of
        RemoteData.NotAsked ->
            text ""
        
        RemoteData.Loading ->
            text "Loading ..."

        RemoteData.Success items ->
            viewItems items
    
        RemoteData.Failure httpError ->
            viewError (buildErrorMessage httpError)


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch items at this time."
    in
        div []
            [ h3 [] [ text errorHeading ]
            , text ("Error: " ++ errorMessage)
            ]


viewItems : List Item -> Html Msg
viewItems items =
    Table.table 
        { options = [ Table.hover, Table.bordered, Table.small ]
        , thead = Table.thead []
            [ Table.tr []
                [ Table.th [] [ text "name" ]
                , Table.th [] [ text "description" ]
                , Table.th [] [ text "unit" ]
                ]
            ]
        , tbody = Table.tbody []
            (List.map viewItem items )
        }

viewItem : Item -> Table.Row Msg
viewItem item  =
    Table.tr [ ]
        [ Table.td [] [ text item.name ]
        , Table.td [] [ text item.description ]
        , Table.td [] [ text item.unit ]
        ]


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


type Msg
    = SendHttpRequest
    | DataReceived (WebData (List Item))


url : String
url =
    "http://localhost:5019/items"


getItems : Cmd Msg
getItems =
    Http.get
        { url = url
        , expect =
            list itemDecoder
            |> Http.expectJson (RemoteData.fromResult >> DataReceived)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( { model | items = RemoteData.Loading }, getItems )

        DataReceived response ->
            ( { model 
              | items = response
              }
            , getItems )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { items = RemoteData.Loading }
    , getItems
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }