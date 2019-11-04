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


type alias Model =
    { items : WebData (List Item)
    , itemSelected : Maybe Item
    , mode : Mode
    , error : String
    }


type Msg
    = MsgFetch
    | MsgReceived (WebData (List Item))
    | MsgSelect Item
    | MsgNew Item
    | MsgEdit Item
    | MsgDeselect Item
    | MsgDeleteConfirm Item
    | MsgDelete Item
    | MsgUpdate Item
    | MsgInsert Item
    | MsgCancel
    | ItemDeleted (Result Http.Error Item)
    | ItemInserted (Result Http.Error Item)
    | ItemUpdated (Result Http.Error Item)
    | StoreName String 
    | StoreDescription String 
    | StoreUnit String 


type Mode
    = ModeChoose
    | ModeSelect
    | ModeEdit
    | ModeDelete
    | ModeNew
    -- | ModeError


init : ( Model, Cmd Msg )
init =
    ( { items = RemoteData.Loading
      , itemSelected  = Nothing
      , mode = ModeChoose
      , error = ""
      }
    , getItems
    )


-- VIEWS

view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , div []
            [ button [ onClick MsgFetch ]
                [ text "Refresh data" ]
            , text model.error 
            , viewItems model.mode model.itemSelected model.items
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


editorButton : Button -> Item -> Html Msg
editorButton button itemSelected =
    case button of
        ButtonAdd -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.primary, Button.onClick (MsgNew emptyItem) ] [ text "Insert" ]

        ButtonEdit -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.primary, Button.onClick (MsgEdit itemSelected) ] [ text "Edit" ]

        ButtonDelete -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.danger, Button.onClick (MsgDelete itemSelected) ] [ text "Delete" ]

        ButtonDeleteConfirm -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.danger, Button.onClick (MsgDeleteConfirm itemSelected) ] [ text "Delete" ]

        ButtonUpdate -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.success, Button.onClick (MsgUpdate itemSelected) ] [ text "Save" ]

        ButtonInsert -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.success, Button.onClick (MsgInsert itemSelected) ] [ text "Save" ]

        ButtonCancel -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.secondary, Button.onClick (MsgCancel) ] [ text "Cancel" ]

        ButtonOk -> Button.button [ Button.small, Button.attrs( [Spacing.mr2]), Button.primary, Button.onClick (MsgCancel) ] [ text "Ok" ]

    
editorButtons : Mode -> Maybe Item -> Html Msg
editorButtons mode itemSelected =
    case mode of
        ModeChoose ->
            div []
                [ editorButton ButtonAdd emptyItem
                ]

        ModeSelect ->
            case itemSelected of
                Nothing ->
                    div [] [ text "Something wrong" ]
                Just item ->
                    div []
                        [ editorButton ButtonEdit item
                        , editorButton ButtonDelete item
                        , editorButton ButtonCancel item
                        ]
                    
        ModeNew ->
            case itemSelected of
                Nothing ->
                    div [] [ text "Something wrong" ]
                Just item ->
                    div []
                        [ editorButton ButtonInsert item
                        , editorButton ButtonCancel item
                        ]
                    
        ModeEdit ->
            case itemSelected of
                Nothing ->
                    div [] [ text "Something wrong" ]
                Just item ->
                    div [ ]
                        [ editorButton ButtonUpdate item
                        , editorButton ButtonCancel item
                        ]

        ModeDelete ->
            case itemSelected of
                Nothing ->
                    div [] [ text "Something wrong" ]
                Just item ->
                    div []
                        [ editorButton ButtonDeleteConfirm item
                        , editorButton ButtonCancel item
                        ]


viewItems : Mode -> Maybe Item -> WebData (List Item) -> Html Msg
viewItems mode itemSelected items =
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
                    (viewItemsInTable mode itemSelected actualItems)
                    -- (List.map (viewItem model itemIdSelected) (actualItems ++ [ emptyItem ]) )
                }
            , editorButtons mode itemSelected
            ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch posts at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


p_itemIsSelected : Maybe Item -> Item -> Bool
p_itemIsSelected itemSelected item =
    case itemSelected of
        Nothing ->
            False
        
        Just itemSelector ->
            item.id == itemSelector.id


viewItemsInTable : Mode -> Maybe Item -> List (Item) -> List ( Table.Row Msg )
viewItemsInTable mode itemSelected actualItems =
    case mode of
        ModeNew ->
            ( List.filter notDeleted actualItems
              |> List.map (viewItem mode itemSelected)
            )
            ++ [ viewItem mode itemSelected emptyItem ]
    
        _ ->
            ( List.filter notDeleted actualItems
              |> List.map (viewItem mode itemSelected)
            )


viewItem : Mode -> Maybe Item -> Item -> Table.Row Msg
viewItem mode itemSelected item  =
    if p_itemIsSelected itemSelected item then
        case (mode, itemSelected) of
            ( ModeChoose, _ ) ->
                Table.tr [ Table.rowAttr(onClick (MsgSelect item)) ]
                    [ Table.td [] [ text item.name ]
                    , Table.td [] [ text item.description ]
                    , Table.td [] [ text item.unit ]
                    ]

            ( ModeSelect, _ ) ->
                Table.tr [ Table.rowPrimary , Table.rowAttr(onClick (MsgDeselect item)) ]
                    [ Table.td [] [ text item.name ]
                    , Table.td [] [ text item.description ]
                    , Table.td [] [ text item.unit ]
                    ]

            ( ModeEdit, Just itemSelected1 ) ->
                Table.tr [ Table.rowActive ]
                    [ Table.td [] [ Input.text [ Input.onInput StoreName, Input.attrs [ Attributes.placeholder "name", Attributes.value itemSelected1.name] ] ]
                    , Table.td [] [ Input.text [ Input.onInput StoreDescription, Input.attrs [ Attributes.placeholder "description", Attributes.value itemSelected1.description ] ] ]
                    , Table.td [] [ Input.text [ Input.onInput StoreUnit, Input.attrs [ Attributes.placeholder "unit", Attributes.value itemSelected1.unit ] ] ]
                    ]

            ( ModeDelete, Just itemSelected1 ) ->
                Table.tr [ Table.rowDanger , Table.rowAttr(onClick (MsgSelect item)) ]
                    [ Table.td [] [ text item.name ]
                    , Table.td [] [ text item.description ]
                    , Table.td [] [ text item.unit ]
                    ]

            ( ModeNew, Just itemSelected1 ) ->
                Table.tr [ Table.rowActive ]
                    [ Table.td [] [ Input.text [ Input.onInput StoreName, Input.attrs [ Attributes.placeholder "name" ] ] ] 
                    , Table.td [] [ Input.text [ Input.onInput StoreDescription, Input.attrs [ Attributes.placeholder "description" ] ] ]
                    , Table.td [] [ Input.text [ Input.onInput StoreUnit, Input.attrs [ Attributes.placeholder "unit" ] ] ]
                    ]
            ( _, _ ) ->
                Table.tr [] []
            
    else
        -- Table.tr [ ]
        Table.tr [ Table.rowAttr(onDoubleClick (MsgEdit item)) , Table.rowAttr(onClick (MsgSelect item)) ]
            [ Table.td [] [ text item.name ]
            , Table.td [] [ text item.description ]
            , Table.td [] [ text item.unit ]
            ]


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
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
                            , error = "itemIdString = " ++ idToString item.id
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

        StoreName value ->
            case model.itemSelected of
                Just item ->
                    ( { model
                    | itemSelected = Just (storeName item value)
                    }
                    , Cmd.none )

                _ ->
                    ( model , Cmd.none )
        
        StoreDescription value ->
            case model.itemSelected of
                Just item ->
                    ( { model
                    | itemSelected = Just (storeDescription item value)
                    }
                    , Cmd.none )

                _ ->
                    ( model , Cmd.none )
        
        StoreUnit value ->
            case model.itemSelected of
                Just item ->
                    ( { model
                    | itemSelected = Just (storeUnit item value)
                    }
                    , Cmd.none )

                _ ->
                    ( model , Cmd.none )
        

-- DATABASE

baseUrl : String
baseUrl =
    "http://localhost:5019/items"


getItems : Cmd Msg
getItems =
    Http.get
        { url = baseUrl
        , expect =
            itemsDecoder
            |> Http.expectJson (RemoteData.fromResult >> MsgReceived)
        }


deleteItem : Item -> Cmd Msg
deleteItem item =
    let
        requestUrl = 
            baseUrl ++ "/" ++ idToString item.id
        item1 = isDeleted item
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

insertItem : Item -> Cmd Msg
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

updateItem : Item -> Cmd Msg
updateItem item =
    let
        requestUrl = 
            baseUrl ++ "/" ++ idToString item.id
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