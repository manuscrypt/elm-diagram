module Extra.Http exposing (httpErrorToString)

import Http

httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.Timeout ->
            "Connection has timed out"

        Http.BadResponse code _ ->
            if code == 401 then
                "Wrong username or password"
            else
                "Some error has occurred on the server"

        Http.NetworkError ->
            "A network error has occurred"

        Http.UnexpectedPayload string ->
            "Unknown error has occurred: " ++ string
 