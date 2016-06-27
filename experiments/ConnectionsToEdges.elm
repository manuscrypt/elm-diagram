import Html
import Html.App as App
import Html.Attributes as HA
import String

nodes = [ "a", "b", "c" ]

connectionsToNodes : List String -> List String

connectionsToNodes nodes =
    case nodes of
        a::b::_ -> [ a ++ "-" ++ b ] ++ ( connectionsToNodes ( List.drop 1 nodes ) )
        _  -> []
--        a::b::_ -> [ a ++ "-" ++ b ] ++ ( connectionsToNodes ( List.drop 1 nodes ) ) 


--    let first = List.head nodes
--    in
--    case first of
--    Nothing -> []
--    Just firstNode -> 
--        let weitere = List.drop 1 nodes
--        in [ firstNode ] ++ weitere

--"a" "b" "c"
--"a-b" "b-c"


edges : List String
edges = connectionsToNodes nodes

text2 = List.map ( \x -> "'" ++ x ++ "'" )

-- ( String.join "aa" text2 )
asText : String
asText = String.join ", " edges

main = Html.text asText 
