module Main exposing (main)
import Browser
import Http
import Html exposing (Html)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput, onClick)
import MovieDB exposing (ApiKey, MovieId, Movie, Keyword)
import Task

-- TODO Separate to different files
-- TODO Add win possibility :)
-- TODO Add css, make it look nicer 
-- TODO Add Readme.md, explain how to get id
-- TODO Add random movie (hardcoded for now)

type alias Gues = (Movie, List Keyword)
type alias State = 
    { input : String
    , gueses : List Gues
    , gues : Maybe Movie
    , correct : MovieId
    , correctKeywords: List Keyword
    }
type StateMsg = Change String | Submit | PeekGues (Result Http.Error (List Movie)) | AddGues (Maybe Gues)

type ModelPart = Loading | GotRandomMovie Int | Success State
type alias Model = 
    { apiKey : ApiKey
    , model : ModelPart
    }

type Msg = ChangeApiKey String | RandomMovie Int | CorrectKeywords (Result Http.Error (List Keyword)) | ChangeState StateMsg

main : Program () Model Msg
main = Browser.element
    { init = \_ -> (Model "" Loading, randomMovie)
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }


randomMovie : Cmd Msg
randomMovie = RandomMovie 605 |> Task.succeed |> Task.perform identity

updateState : ApiKey -> StateMsg -> State -> (State, Cmd StateMsg)
updateState apiKey msg state = case msg of
    Change q -> ({ state | input = q }, if String.isEmpty q then Cmd.none else Cmd.map PeekGues <| MovieDB.search apiKey q)
    PeekGues g -> ({ state | gues = Result.toMaybe g |> Maybe.andThen List.head }, Cmd.none)
    Submit -> ({ state | input = "", gues = Nothing }, Maybe.withDefault Cmd.none <| Maybe.map (addGues apiKey) state.gues)
    AddGues g -> case g of
       Just gues -> ({ state | gueses = gues :: state.gueses }, Cmd.none)
       Nothing -> (state, Cmd.none)

addGues : ApiKey -> Movie -> Cmd StateMsg
addGues apiKey m =  MovieDB.movieToKeywords apiKey m.id |> Cmd.map (Result.toMaybe >> Maybe.map (\kws -> (m, kws)) >> AddGues)

update : Msg -> Model -> (Model, Cmd Msg)
update msg { apiKey, model } = case msg of
    ChangeApiKey key -> (Model key model, randomMovie)
    RandomMovie id -> (Model apiKey <| GotRandomMovie id, Cmd.map CorrectKeywords <| MovieDB.movieToKeywords apiKey id)
    CorrectKeywords rws -> case model of
        GotRandomMovie id ->
            case rws of
                Ok kws -> (Model apiKey <| Success <| State "" [] Nothing id kws, Cmd.none)
                Err _ -> (Model apiKey <| model, Cmd.none)
        _ -> Debug.todo "Imposible"
    ChangeState stateMsg ->
        case model of
            Success state ->
                let (st, cmd) = updateState apiKey stateMsg state
                in (Model apiKey <| Success st, Cmd.map ChangeState cmd)
            _ -> Debug.todo "Imposible"


viewInputGues : State -> Html Msg
viewInputGues state =  Html.div []
    [ Html.input [ placeholder "Gues movie...", value state.input, onInput <| ChangeState << Change ] []
    , Html.button[ onClick <| ChangeState <| Submit ] [ Html.text "Submit" ]
    , Html.text <| Maybe.withDefault "???" <| Maybe.map .name state.gues
    ]

getSame : List a -> List a -> List a
getSame xs ys = List.partition (\x -> List.member x xs) ys |> Tuple.first 

viewGues : List Keyword -> Gues -> List (Html Msg)
viewGues correctKws (movie, kws) = (Html.text <| movie.name ++ ":    ")
    :: List.map (.name >> (\s -> s ++ ",  ") >> Html.text) (getSame correctKws kws)

view : Model -> Html Msg
view { apiKey, model } = Html.div []
    [ Html.input [ placeholder "ApiKey...", value apiKey, onInput <| ChangeApiKey ] []
    , viewModelPart model
    ]

viewModelPart : ModelPart -> Html Msg
viewModelPart model = case model of
    Success state -> Html.div [] <|
        (Html.li [] [viewInputGues state])
        :: List.map (viewGues state.correctKeywords >> Html.li [  ]) state.gueses
    _ -> Html.text "Loading...."
