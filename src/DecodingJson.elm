module DecodingJson exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing 
    ( Decoder
    , decodeString
    , field
    , int
    , list
    -- , map3
    , string)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import RemoteData exposing (RemoteData, WebData)

type alias Post =
    { id : Int
    , title : String
    , author : String
    }

type alias Model =
    { posts : WebData (List Post)
    }

view : Model -> Html Msg
view model = 
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Refresh posts" ]
        , viewPostsOrError model
        ]

viewPostsOrError : Model -> Html Msg
viewPostsOrError model =
    case model.posts of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            text "loading ..."
        
        RemoteData.Success posts ->
            viewPosts posts
        
        RemoteData.Failure httpError ->
            viewError (buildErrorMessage httpError)

viewError : String -> Html Msg
viewError errorMessage = 
    let
        errorHeading = "Couldn't get the data from the server"
    in
    div []
        [ h3 [] [text errorHeading ]
        , text ("Error : " ++ errorMessage )
        ]

viewPosts : List Post -> Html Msg
viewPosts posts =
    div []
        [ h3 [] [ text "Posts" ]
        , table []
            ([ viewTableHeader ] ++ List.map viewPost posts)
        ]

viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "Id" ]
        , th []
            [ text "Title" ]
        , th []
            [ text "Author" ]
        ]

viewPost : Post -> Html Msg
viewPost post = 
    tr []
        [ td []
            [ text (String.fromInt post.id ) ]
        , td []
            [ text post.title ]
        , td []
            [ text post.author ]
        ]

type Msg =
    SendHttpRequest
    | DataReceived (WebData (List Post))

postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" int
        |> required "title" string
        |> required "author" string

fetchPosts  : Cmd Msg
fetchPosts =
    Http.get 
        { url = "http://localhost:5019/posts"
        , expect = 
            list postDecoder
            |> Http.expectJson (RemoteData.fromResult >> DataReceived)
        }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( { model | posts = RemoteData.Loading } , fetchPosts )
        
        DataReceived response ->
            ( { model 
              | posts = response
              }
            , Cmd.none
            )

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

init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = RemoteData.Loading
      }
    , fetchPosts
    )

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }