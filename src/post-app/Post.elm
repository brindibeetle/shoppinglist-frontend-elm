module Post exposing (idParser, Post, PostId, postDecoder, postsDecoder, postEncoder, idToString, emptyPost, newPostEncoder)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Url.Parser exposing (Parser, custom)
import Json.Encode as Encode

type alias Post = 
    { id : PostId
    , title : String
    , authorName : String
    , authorUrl : String
    }

postsDecoder : Decoder (List Post)
postsDecoder =
    list postDecoder

postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
    |> required "id" idDecoder
    |> required "title" string
    |> required "authorName" string
    |> required "authorUrl" string

type PostId 
    = PostId Int

idDecoder : Decoder PostId
idDecoder =
    Decode.map PostId int


idToString : PostId -> String
idToString (PostId postId) =
    String.fromInt postId


idParser : Parser (PostId -> a) a
idParser =
    custom "POSTID" <|
        \postId ->
            Maybe.map PostId (String.toInt postId)


postEncoder : Post -> Encode.Value
postEncoder post =
    Encode.object
        [ ( "id", encodeId post.id )
        , ( "title", Encode.string post.title )
        , ( "authorName", Encode.string post.authorName )
        , ( "authorUrl", Encode.string post.authorUrl )
        ]


newPostEncoder : Post -> Encode.Value
newPostEncoder post =
    Encode.object
        [ ( "title", Encode.string post.title )
        , ( "authorName", Encode.string post.authorName )
        , ( "authorUrl", Encode.string post.authorUrl )
        ]


encodeId : PostId -> Encode.Value
encodeId (PostId id) =
    Encode.int id


emptyPost : Post
emptyPost =
    { id = emptyPostId
    , title = ""
    , authorName = ""
    , authorUrl = ""
    }


emptyPostId : PostId
emptyPostId =
    PostId -1

