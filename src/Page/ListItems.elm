        -- TO DO : RENAME Store  eg StoreName : DataBase, Save : Model

module Page.ListItems exposing (..)

import Domain.Item exposing (..)
import Http
import Html exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput, onBlur)
import Html.Attributes as Attributes
import Browser
import Array exposing (..)
import Bootstrap.Table as Table
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.CDN as CDN
import Bootstrap.Utilities.Spacing as Spacing
import Json.Decode as Decode exposing (Decoder, Error(..), decodeString, list, int, string)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import RemoteData exposing (RemoteData, WebData, succeed)
import Error exposing (buildErrorMessage)

-- CONFIGURATION

type Config data =
  Config
    { toId : data -> Int
    , storeId : Int -> data -> data
    , empty : data
    , columns : List (ColumnData data )
    }

config
  : { toId : data -> Int
    , storeId : Int -> data -> data
    , empty : data
    , columns : List (Column data)
    }
  -> Config data
config { toId, storeId, empty, columns } =
  Config
    { toId = toId
    , storeId = storeId
    , empty = empty
    , columns = List.map (\(Column cData) -> cData) columns
    }

type Column data =
  Column (ColumnData data )


type alias ColumnData data  =
  { name : String
  , viewData : data -> HtmlDetails (Msg data)
  , editData : data -> HtmlDetails (Msg data)
  , storeData : String -> data -> data
  }


{-|-}
stringColumn : String -> (data -> String) -> (String -> data -> data) -> Column data
stringColumn name toStr storeData =
  Column
    { name = name
    , viewData = textDetails << toStr
    , editData = \value -> inputDetails (toStr value) storeData name
    , storeData = storeData
    }


-- {-|-}
-- intColumn : String -> (data -> Int) -> Column data msg
-- intColumn name toInt =
--   Column
--     { name = name
--     , viewData = textDetails << toString << toInt
--     , storeData = storeData
--     }



textDetails : String -> HtmlDetails msg
textDetails str =
  HtmlDetails [] [ Html.text str ]

inputDetails : String -> (String -> data -> data) -> String -> HtmlDetails (Msg data)
inputDetails str store placeholder =
    HtmlDetails [] [ Input.text [ Input.onInput (Store store), Input.attrs [ Attributes.placeholder placeholder, Attributes.value str] ] ]

type alias HtmlDetails msg =
  { attributes : List (Table.CellOption msg)
  , children : List (Html msg)
  }

-- IMPLEMENT CONFIG

configItem : Config Item
configItem =
  config
    { toId = .id
    , storeId = storeId
    , empty = emptyItem
    , columns =
        [ stringColumn "Name" .name storeName
        , stringColumn "Description" .description storeDescription
        , stringColumn "Unit" .unit storeUnit
        ]
    }

-- PROGRAM

type alias Model =
    { items : WebData (List Item)
    , itemSelected : Maybe Item
    , mode : Mode
    , error : String
    }


type Msg data
    = MsgFetch
    | MsgReceived (WebData (List data))
    | MsgSelect data
    | MsgNew data
    | MsgEdit data
    | MsgDeselect data
    | MsgDeleteConfirm data
    | MsgDelete data
    | MsgUpdate data
    | MsgInsert data
    | MsgCancel
    | ItemDeleted (Result Http.Error data)
    | ItemInserted (Result Http.Error data)
    | ItemUpdated (Result Http.Error data)
    | Store (String -> data -> data) String


type Mode
    = ModeChoose
    | ModeSelect
    | ModeEdit
    | ModeDelete
    | ModeNew
    -- | ModeError


init : ( Model, Cmd (Msg Item))
init =
    ( { items = RemoteData.Loading
      , itemSelected  = Nothing
      , mode = ModeChoose
      , error = ""
      }
    , getItems
    )


-- VIEWS

view : Model -> Html (Msg Item)
view model =
    Grid.container []
        [ CDN.stylesheet
        , div []
            [ button [ onClick MsgFetch ]
                [ text "Refresh data" ]
            , text model.error 
            , viewItems configItem model.mode model.itemSelected model.items
            ]
        ]


type Button
    = ButtonAdd
    | ButtonEdit
    | ButtonDelete
    | ButtonDeleteConfirm
    | ButtonUpdate
    | ButtonInsert
    | ButtonCancel
    | ButtonOk


editorButton : Button -> data -> Html (Msg data)
editorButton button dataSelected =
    case button of
        ButtonAdd -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.primary, Button.onClick (MsgNew dataSelected) ] [ text "Insert" ]

        ButtonEdit -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.primary, Button.onClick (MsgEdit dataSelected) ] [ text "Edit" ]

        ButtonDelete -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.danger, Button.onClick (MsgDelete dataSelected) ] [ text "Delete" ]

        ButtonDeleteConfirm -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.danger, Button.onClick (MsgDeleteConfirm dataSelected) ] [ text "Delete" ]

        ButtonUpdate -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.success, Button.onClick (MsgUpdate dataSelected) ] [ text "Save" ]

        ButtonInsert -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.success, Button.onClick (MsgInsert dataSelected) ] [ text "Save" ]

        ButtonCancel -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.secondary, Button.onClick (MsgCancel) ] [ text "Cancel" ]

        ButtonOk -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.primary, Button.onClick (MsgCancel) ] [ text "Ok" ]

    
editorButtons : data -> Mode -> Maybe data -> Html (Msg data)
editorButtons empty mode dataSelected =
    case mode of
        ModeChoose ->
            div []
                [ editorButton ButtonAdd empty
                ]

        ModeSelect ->
            case dataSelected of
                Nothing ->
                    div [] [ text "Something wrong" ]
                Just datarow ->
                    div []
                        [ editorButton ButtonEdit datarow
                        , editorButton ButtonDelete datarow
                        , editorButton ButtonCancel datarow
                        ]
                    
        ModeNew ->
            case dataSelected of
                Nothing ->
                    div [] [ text "Something wrong" ]
                Just datarow ->
                    div []
                        [ editorButton ButtonInsert datarow
                        , editorButton ButtonCancel datarow
                        ]
                    
        ModeEdit ->
            case dataSelected of
                Nothing ->
                    div [] [ text "Something wrong" ]
                Just datarow ->
                    div [ ]
                        [ editorButton ButtonUpdate datarow
                        , editorButton ButtonCancel datarow
                        ]

        ModeDelete ->
            case dataSelected of
                Nothing ->
                    div [] [ text "Something wrong" ]
                Just datarow ->
                    div []
                        [ editorButton ButtonDeleteConfirm datarow
                        , editorButton ButtonCancel datarow
                        ]


viewItems : Config data -> Mode -> Maybe data -> WebData (List data) -> Html (Msg data)
viewItems ( Config {toId, storeId, empty, columns} ) mode itemSelected items =
    case items of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success actualItems ->
            div []
            [ Table.table 
                { options = [ Table.hover, Table.bordered, Table.small ]
                , thead = Table.thead []
                    [ Table.tr [ ]
                        [ Table.th [] [ text "name" ]
                        , Table.th [] [ text "description" ]
                        , Table.th [] [ text "unit" ]
                        ]
                    ]
                , tbody = Table.tbody []
                    (viewItemsInTable columns toId empty mode itemSelected actualItems)
                    -- (List.map (viewItem model itemIdSelected) (actualItems ++ [ emptyItem ]) )
                }
            , editorButtons empty mode itemSelected
            ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewFetchError : String -> Html (Msg data)
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch posts at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


p_itemIsSelected : (data -> Int) -> Maybe data -> data -> Bool
p_itemIsSelected toId dataSelected datarow =
    case dataSelected of
        Nothing ->
            False
        
        Just datarowSelected ->
            toId datarow == toId datarowSelected


viewItemsInTable : List (ColumnData data ) -> (data -> Int) -> data -> Mode -> Maybe data -> List (data) -> List ( Table.Row (Msg data) )
viewItemsInTable columns toId empty mode datarowSelected datarows =
    case mode of
        ModeNew ->
            ( List.filter (notDeleted <| toId) datarows
              |> List.map (tableRow columns toId mode datarowSelected)
            )
            ++ [ tableRow columns toId mode datarowSelected empty ]
    
        _ ->
            ( List.filter (notDeleted <| toId) datarows
              |> List.map (tableRow columns toId  mode datarowSelected)
            )


tableRow : List (ColumnData data ) -> (data -> Int) -> Mode -> Maybe data -> data -> Table.Row (Msg data)
tableRow columns toId mode dataSelected datarow  =
    if p_itemIsSelected toId dataSelected datarow then
        case (mode, dataSelected) of
            ( ModeChoose, _ ) ->
                tablerowReadonly [ Table.rowAttr(onClick (MsgSelect datarow)) ] columns datarow

            ( ModeSelect, _ ) ->
                tablerowReadonly [ Table.rowPrimary , Table.rowAttr(onClick (MsgDeselect datarow)) ] columns datarow

            ( ModeEdit, Just datarowSelected ) ->
                tablerowEdit [ Table.rowActive ] columns datarowSelected

            ( ModeDelete, Just datarowSelected ) ->
                tablerowReadonly [ Table.rowDanger , Table.rowAttr(onClick (MsgSelect datarow)) ] columns datarow

            ( ModeNew, Just datarowSelected ) ->
                tablerowEdit [ Table.rowActive ] columns datarowSelected

            ( _, _ ) ->
                Table.tr [] []
            
    else
        tablerowReadonly [ Table.rowAttr(onDoubleClick (MsgEdit datarow)) , Table.rowAttr(onClick (MsgSelect datarow)) ] columns datarow


tablerowReadonly : List (Table.RowOption (Msg data)) -> List (ColumnData data ) -> data -> Table.Row (Msg data)
tablerowReadonly attributes columns datarow =
    Table.tr attributes (List.map (viewDatacellReadonly datarow) columns)


viewDatacellReadonly : data -> ColumnData data -> Table.Cell (Msg data)
viewDatacellReadonly datarow {viewData} =
  let
    details =
      viewData datarow
  in
    Table.td details.attributes details.children


tablerowEdit : List (Table.RowOption (Msg data)) -> List (ColumnData data) -> data -> Table.Row (Msg data)
tablerowEdit attributes columns datarow =
    Table.tr attributes (List.map (viewDatacellEdit datarow) columns)


viewDatacellEdit : data -> ColumnData data -> Table.Cell (Msg data)
viewDatacellEdit datarow {editData} =
  let
    details =
      editData datarow
  in
    Table.td details.attributes details.children


notDeleted : (data -> Int) -> data -> Bool
notDeleted toId datarow =
    toId datarow > 0

isDeleted : data -> (data -> Int) -> (data -> Int -> data) -> data
isDeleted datarow toId storeId =
    storeId datarow (0 - (toId datarow))

-- UPDATE

update : (Msg Item) -> Model -> ( Model, Cmd (Msg Item) )
update msg model =
    case msg of
        MsgFetch ->
            ( { model 
              | items = RemoteData.Loading
              }
              , getItems )

        MsgReceived response ->
            ( { model 
              | items = response
              , mode = ModeChoose
              }
            , Cmd.none )

        MsgSelect item ->
            ( { model
            | itemSelected = Just item
            , mode = ModeSelect
            }
            , Cmd.none)

        MsgDeselect item ->
            ( { model
            | itemSelected = Nothing
            , mode = ModeChoose
            }
            , Cmd.none)

        MsgNew item ->
           ( { model 
              | itemSelected = Just item
              , mode = ModeNew
              }
            , Cmd.none )

        MsgEdit item ->
            ( { model 
              | mode = ModeEdit
              }
            , Cmd.none )

        MsgDelete item ->
            ( { model 
              | mode = ModeDelete
              }
            , Cmd.none )

        MsgDeleteConfirm item ->
            (model , deleteItem item )
            
        MsgUpdate item ->
            (model , updateItem item )

        MsgInsert item ->
            (model , insertItem item )

        MsgCancel ->
            ( { model 
              | mode = ModeChoose
              , itemSelected = Nothing
              }
            , Cmd.none )

        ItemDeleted result_error_item ->
            case result_error_item of
                Result.Err err ->
                    ( { model 
                    | error = "ItemDeleted Result.Err error : " ++ buildErrorMessage err
                    }
                    , Cmd.none )

                Result.Ok item ->  
                    case model.items of
                        RemoteData.Success items ->
                            ( { model 
                            | mode = ModeChoose
                            , items = RemoteData.succeed (selectUpdateItem item items)
                            , itemSelected = Nothing
                            , error = "itemIdString = " ++ String.fromInt item.id
                            }
                            , Cmd.none )
                        httpError ->
                            ( { model 
                            | error = "case model.items of RemoteData.Success items -> "
                            }
                            , Cmd.none )

        ItemInserted result_error_item ->
            case result_error_item of
                Result.Err error ->
                    ( model, Cmd.none )

                Result.Ok item ->
                    case model.items of
                        RemoteData.Success items ->
                            ( { model 
                            | mode = ModeChoose
                            , items = RemoteData.succeed (item :: items)
                            , itemSelected = Nothing
                            }
                            , Cmd.none )
                        _ ->
                            ( model, Cmd.none )

        ItemUpdated result_error_item ->
            case result_error_item of
                Result.Err error ->
                    ( model, Cmd.none )

                Result.Ok item ->
                    case model.items of
                        RemoteData.Success items ->
                            ( { model 
                            | mode = ModeChoose
                            , items = RemoteData.succeed (selectUpdateItem item items)
                            , itemSelected = Nothing
                            }
                            , Cmd.none )
                        _ ->
                            ( model, Cmd.none )

        Store store value ->
            case model.itemSelected of
                Just item ->
                    ( { model
                    | itemSelected = Just (store value item)
                    }
                    , Cmd.none )

                _ ->
                    ( model , Cmd.none )

-- DATABASE

baseUrl : String
baseUrl =
    "http://localhost:5019/items"


getItems : Cmd (Msg Item)
getItems =
    Http.get
        { url = baseUrl
        , expect =
            itemsDecoder
            |> Http.expectJson (RemoteData.fromResult >> MsgReceived)
        }


deleteItem : Item -> Cmd (Msg Item)
deleteItem item =
    let
        requestUrl = 
            baseUrl ++ "/" ++ String.fromInt item.id
        item1 = item
        -- item1 = isDeleted item
    in
        Http.request
            { method = "PATCH"
            , headers = []
            , url = requestUrl
            , body = Http.jsonBody (itemEncoder item1)
            , expect = Http.expectJson ItemDeleted itemDecoder
            , timeout = Nothing
            , tracker = Nothing
            }


-- deleteItem : Item -> Cmd Msg
-- deleteItem item =
--     let
--         requestUrl = 
--             baseUrl ++ "/" ++ idToString item.id
--     in
--         Http.request
--             { method = "DELETE"
--             , headers = []
--             , url = requestUrl
--             , body = Http.jsonBody (itemIdEncoder item.id)
--             , expect = Http.expectString ItemDeleted 
--             , timeout = Nothing
--             , tracker = Nothing
--             }

insertItem : Item -> Cmd (Msg Item)
insertItem item =
    let
        requestUrl = 
            baseUrl
    in
        Http.post
            { url = requestUrl
            , body = Http.jsonBody (newItemEncoder item)
            , expect = Http.expectJson ItemInserted itemDecoder
            }

updateItem : Item -> Cmd (Msg Item)
updateItem item =
    let
        requestUrl = 
            baseUrl ++ "/" ++ String.fromInt item.id
    in
        Http.request
            { method = "PATCH"
            , headers = []
            , url = requestUrl
            , body = Http.jsonBody (itemEncoder item)
            , expect = Http.expectJson ItemUpdated itemDecoder
            , timeout = Nothing
            , tracker = Nothing
            }


-- UTILS

selectUpdateItem : Item -> List Item -> List Item
selectUpdateItem itemSelected items =
    List.map (selectSaveItem itemSelected) items