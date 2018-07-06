module Main exposing (..)

import Html as H
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodeP


---- MODEL ----


type alias Model =
    { todoList : TodoListState
    , titleInput : String
    }


type TodoListState
    = TodoListLoading
    | TodoListLoadingError String
    | TodoListLoadingSuccess (List Todo)


type alias Todo =
    { title : String
    , completed : Bool
    , id : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { titleInput = ""
      , todoList = TodoListLoading
      }
    , Http.send TodoListLoaded fetchTodoList
    )


fetchTodoList : Http.Request (List Todo)
fetchTodoList =
    Http.get "http://localhost:3030/todos" todoListDecoder


todoListDecoder : Decode.Decoder (List Todo)
todoListDecoder =
    Decode.list todoDecoder


todoDecoder : Decode.Decoder Todo
todoDecoder =
    DecodeP.decode Todo
        |> DecodeP.required "title" Decode.string
        |> DecodeP.required "completed" Decode.bool
        |> DecodeP.required "id" Decode.int



---- UPDATE ----


type Msg
    = ToggleTodo Int
    | TitleInputChanged String
    | AddTodo
    | TodoListLoaded (Result Http.Error (List Todo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleTodo todoId ->
            case model.todoList of
                TodoListLoading ->
                    ( model, Cmd.none )

                TodoListLoadingError string ->
                    ( model, Cmd.none )

                TodoListLoadingSuccess todoList ->
                    ( { model
                        | todoList =
                            TodoListLoadingSuccess
                                (todoList
                                    |> List.map
                                        (\todo ->
                                            if todo.id == todoId then
                                                { todo
                                                    | completed = not todo.completed
                                                }
                                            else
                                                todo
                                        )
                                )
                      }
                    , Cmd.none
                    )

        TitleInputChanged value ->
            ( { model | titleInput = value }, Cmd.none )

        AddTodo ->
            case model.todoList of
                TodoListLoading ->
                    ( model, Cmd.none )

                TodoListLoadingError string ->
                    ( model, Cmd.none )

                TodoListLoadingSuccess todoList ->
                    ( { model
                        | titleInput = ""
                        , todoList =
                            TodoListLoadingSuccess
                                (List.concat
                                    [ todoList
                                    , [ { title = model.titleInput
                                        , id = List.length todoList + 1
                                        , completed = False
                                        }
                                      ]
                                    ]
                                )
                      }
                    , Cmd.none
                    )

        TodoListLoaded result ->
            case result of
                Err error ->
                    ( { model | todoList = TodoListLoadingError "loading todos failed" }, Cmd.none )

                Ok todoList ->
                    ( { model | todoList = TodoListLoadingSuccess todoList }, Cmd.none )



---- VIEW ----


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.h1 [] [ H.text "Create todo:" ]
        , H.form [ E.onSubmit AddTodo ]
            [ H.label [] [ H.text "title:" ]
            , H.input [ A.value model.titleInput, E.onInput TitleInputChanged ] []
            , H.button [ A.type_ "submit" ] [ H.text "Add todo" ]
            ]
        , H.h1 [] [ H.text "Todo list" ]
        , case model.todoList of
            TodoListLoading ->
                H.div [] [ H.text "List loading..." ]

            TodoListLoadingError error ->
                H.div [] [ H.text error ]

            TodoListLoadingSuccess todoList ->
                H.div [] (todoList |> List.map todoItemView)
        ]


todoItemView : Todo -> H.Html Msg
todoItemView todo =
    H.p
        ([ A.class "todo-item"
         , E.onClick (ToggleTodo todo.id)
         ]
            ++ if todo.completed then
                [ A.class "completed" ]
               else
                []
        )
        [ H.text todo.title ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    H.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
