module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, div, text, a, h1, h2, header, nav, span, main_)
import Html.Attributes exposing (href, class, id)
import Http
import Json.Decode exposing (Decoder, list, field, string, map2)



main : Program () Model Msg
main =
  Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type State
    = Failure
    | Loading
    | Success Data

type alias Model =
    { zennTechState : State
    , zennIdeasState : State
    , zennBooksState : State
    , qiitaState : State
    }

type alias Data =
    ( List String, List String )

zennDecoder : Decoder Data
zennDecoder = map2 (\title path -> ( title, path )) (list (field "title" string)) (list (field "path" string))
qiitaDecoder : Decoder Data
qiitaDecoder = map2 (\title path -> ( title, path )) (list (field "node" (field "title" string))) (list (field "node" (field "linkUrl" string)))

init : () -> (Model, Cmd Msg)
init _ =
    ( { zennTechState = Loading
      , zennIdeasState = Loading
      , zennBooksState = Loading
      , qiitaState = Loading
      }
      , Cmd.batch
      [ Http.get
      { url = "https://zenn-api.vercel.app/api/trendTech"
      , expect = Http.expectJson GotZennTech zennDecoder
      }
      , Http.get
      { url = "https://zenn-api.vercel.app/api/trendIdea"
      , expect = Http.expectJson GotZennIdeas zennDecoder
      }
      , Http.get
      { url = "https://zenn-api.vercel.app/api/trendBook"
      , expect = Http.expectJson GotZennBooks zennDecoder
      }
      , Http.get
      { url = "https://qiita-api.vercel.app/api/trend"
      , expect = Http.expectJson GotQiita qiitaDecoder
      }
      ]
    )


type Msg
    = GotZennTech (Result Http.Error Data)
    | GotZennIdeas (Result Http.Error Data)
    | GotZennBooks (Result Http.Error Data)
    | GotQiita (Result Http.Error Data)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotZennTech result ->
        case result of
            Ok data ->
                ( { model | zennTechState = Success data }, Cmd.none )
            Err _ ->
                ( { model | zennTechState = Failure }, Cmd.none )
    GotZennIdeas result ->
        case result of
            Ok data ->
                ( { model | zennIdeasState = Success data }, Cmd.none )
            Err _ ->
                ( { model | zennIdeasState = Failure }, Cmd.none )
    GotZennBooks result ->
        case result of
            Ok data ->
                ( { model | zennBooksState = Success data }, Cmd.none )
            Err _ ->
                ( { model | zennBooksState = Failure }, Cmd.none )
    GotQiita result ->
        case result of
            Ok data ->
                ( { model | qiitaState = Success data }, Cmd.none )
            Err _ ->
                ( { model | qiitaState = Failure }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _= Sub.none

type alias Template = Data -> Html Msg
zennArticles : Template
zennArticles data = div []
    (List.map2 (\title path -> div [class "articles"] [a [href (String.append "https://zenn.dev" path)] [text title]]) (Tuple.first data) (Tuple.second data))

qiitaArticles : Template
qiitaArticles data = div []
    (List.map2 (\title path -> div [class "articles"] [a [href path] [text title]]) (Tuple.first data) (Tuple.second data))

load : Template -> State -> Html Msg
load template state =
    case state of
        Loading ->
            text "LOADING..."
        Failure ->
            text "FAILURE!"
        Success data ->
            template data

type alias Section =
    { template : Template
    , state : State
    , name : String
    , id : String
    }

sections : Model -> List Section
sections model =
    [{ template = zennArticles
    , state = model.zennTechState
    , name = "Zenn Tech"
    , id = "zenn-tech"
    }
    ,{ template = zennArticles
    , state = model.zennIdeasState
    , name = "Zenn Ideas"
    , id = "zenn-ideas"
    }
    ,{ template = zennArticles
    , state = model.zennBooksState
    , name = "Zenn Books"
    , id = "zenn-books"
    }
    ,{ template = qiitaArticles
    , state = model.qiitaState
    , name = "Qiita All"
    , id = "qiita-all"
    }
    ]

view : Model -> Document Msg
view model =
    { title = "Sugoi Trend List with Elm"
    , body =
    [ header []
    [ nav []
        (List.map
            (\section -> a [href (String.append "#" section.id)] [text section.name]) (sections model)
        )
    , h1 []
        [ span [class "sugoi"] [ text "Sugoi" ]
        , text " Trend List with Elm"
        ]
    ]
    , main_ []
        (List.map
            (\section -> Html.section []
                [ h2 [id section.id] [ text section.name ]
                , load (section.template) (section.state)
                ])
                (sections model)
        )
    ]
    }