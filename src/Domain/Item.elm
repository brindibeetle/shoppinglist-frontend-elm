module Domain.Item exposing (Item, ItemId, itemFromName, itemDecoder, itemEncoder, newItemEncoder,itemsDecoder, emptyItem, emptyItemId
    , idToString, idFromString, itemIdDecoder, itemIdEncoder, isDeleted, notDeleted
    , selectSaveItem, storeName, storeDescription, storeUnit)

import Array exposing (Array)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)

import Http

type alias Item = 
    {
        id : Int
        , name : String
        , description : String
        , unit : String
    }


itemFromName : String -> Item
itemFromName name =
    Item emptyItemId name ""  ""


itemsDecoder : Decoder (List Item)
itemsDecoder =
    list itemDecoder


itemDecoder : Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "id" int
        |> required "name" string
        |> required "description" string
        |> optional "unit" string ""

itemEncoder : Item -> Encode.Value
itemEncoder item =
    Encode.object
        [ ( "id", Encode.int item.id )
        , ( "name", Encode.string item.name )
        , ( "description", Encode.string item.description )
        , ( "unit", Encode.string item.unit )
        ]


newItemEncoder : Item -> Encode.Value
newItemEncoder item =
    Encode.object
        [ ( "name", Encode.string item.name )
        , ( "description", Encode.string item.description )
        , ( "unit", Encode.string item.unit )
        ]


type ItemId = ItemId Int


itemIdDecoder : Decoder ItemId
itemIdDecoder =
    Decode.succeed ItemId
        |> required "id" int


itemIdEncoder : ItemId -> Encode.Value
itemIdEncoder itemId =
    Encode.object
        [ ("id", encodeId itemId)]


idToString : ItemId -> String
idToString (ItemId itemId) =
    String.fromInt itemId


idFromString : String -> ItemId
idFromString itemIdString =
    let
        itemId1 = String.toInt itemIdString
    in
    case itemId1 of
        Just itemId ->
            ItemId itemId
        Nothing ->
            ItemId -9
    

idDecoder : Decoder ItemId
idDecoder =
    Decode.map ItemId int


encodeId : ItemId -> Encode.Value
encodeId (ItemId id) =
    Encode.int id


emptyItem : Item
emptyItem = Item emptyItemId "" "" ""


emptyItemId : Int
emptyItemId =
    -1

selectSaveItem : Item -> Item -> Item
selectSaveItem itemNew itemOld =
    if (itemOld.id /= itemNew.id ) then
        itemOld
    else
        itemNew

-- selectDeleteItem : Item -> Item -> Item

storeName : String -> Item -> Item
storeName value item =
    { item
    | name = value
    }

storeDescription : String -> Item -> Item
storeDescription value item =
    { item
    | description = value
    }

storeUnit : String -> Item -> Item
storeUnit value item =
    { item
    | unit = value
    }


isDeleted : Item -> Item
isDeleted item =
    { item
    | name = "<<DELETED>>"
    }


notDeleted : Item -> Bool
notDeleted item =
    item.name /= "<<DELETED>>"