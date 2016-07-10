module Model.ElmFile exposing (ElmFile, decodeList, encodeList)

import Json.Encode
import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))


type alias ElmFile =
    { id : Int
    , name : String
    , path : String
    , size : Int
    , imports : List String
    , moduleName : String
    }


decodeList : Json.Decode.Decoder (List ElmFile)
decodeList =
    Json.Decode.list decodeElmFile


encodeList : List ElmFile -> Json.Encode.Value
encodeList list =
    Json.Encode.list (List.map encodeElmFile list)


decodeElmFile : Json.Decode.Decoder ElmFile
decodeElmFile =
    Json.Decode.succeed ElmFile
        |: ("id" := Json.Decode.int)
        |: ("name" := Json.Decode.string)
        |: ("path" := Json.Decode.string)
        |: ("size" := Json.Decode.int)
        |: ("imports" := Json.Decode.list Json.Decode.string)
        |: ("moduleName" := Json.Decode.string)


encodeElmFile : ElmFile -> Json.Encode.Value
encodeElmFile record =
    Json.Encode.object
        [ ( "id", Json.Encode.int <| record.id )
        , ( "name", Json.Encode.string <| record.name )
        , ( "path", Json.Encode.string <| record.path )
        , ( "size", Json.Encode.int <| record.size )
        , ( "imports", Json.Encode.list <| List.map Json.Encode.string <| record.imports )
        , ( "moduleName", Json.Encode.string <| record.moduleName )
        ]
