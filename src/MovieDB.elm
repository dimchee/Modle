module MovieDB exposing (ApiKey, MovieId, Movie, Keyword, search, movieToKeywords, keywordToMovies)

import Http
import Json.Decode as D

type alias ApiKey = String
type alias MovieId = Int
type alias Movie = 
    { id: MovieId
    , name: String
    }
type alias Keyword =
    { id: Int
    , name: String
    }



moviesDecoder : D.Decoder (List Movie)
moviesDecoder = D.field "results" <| D.list movieDecoder

movieDecoder : D.Decoder Movie 
movieDecoder =
    D.map2 Movie
        (D.field "id" D.int)
        (D.field "title" D.string)

keywordsDecoder : D.Decoder (List Keyword)
keywordsDecoder = D.field "keywords" <| D.list keywordDecoder

keywordDecoder : D.Decoder Keyword
keywordDecoder =
    D.map2 Keyword
        (D.field "id" D.int)
        (D.field "name" D.string)

-- search movie
-- https://api.themoviedb.org/3/search/movie?api_key=<api_key>&query=matrix
search : ApiKey -> String -> Cmd (Result Http.Error (List Movie))
search apiKey q =
  Http.get
    -- maybe esquape q? ie. ' ' -> '+'
    { url = "https://api.themoviedb.org/3/search/movie?api_key="
        ++ apiKey ++ "&query=" ++ q
    , expect = Http.expectJson identity moviesDecoder
    }

-- get keywords for movie
-- https://api.themoviedb.org/3/movie/624860/keywords?api_key=<api_key>
movieToKeywords : ApiKey -> MovieId -> Cmd (Result Http.Error (List Keyword))
movieToKeywords apiKey movieId =
    Http.get
        { url = "https://api.themoviedb.org/3/movie/"
            ++ String.fromInt movieId
            ++ "/keywords?api_key=" ++ apiKey
        , expect = Http.expectJson identity keywordsDecoder
        }


-- get movies with keyword
-- https://api.themoviedb.org/3/keyword/295338/movies?api_key=<api_key>
keywordToMovies : ApiKey -> Keyword -> Cmd (Result Http.Error (List Movie))
keywordToMovies apiKey k =
    Http.get
        { url = "https://api.themoviedb.org/3/keyword/"
            ++ String.fromInt k.id
            ++ "/movies?api_key=" ++ apiKey 
        , expect = Http.expectJson identity moviesDecoder
        }
