import Html
import Html.App as App
import Html.Attributes as HA
import String

type CU = 
    { name : String
    }

first : CU
first = CU "first"

main = Html.text CU.name 
