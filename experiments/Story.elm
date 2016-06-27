module Story exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, scale)
import Time exposing (Time)
import Html
import Svg
import Svg.Attributes as SA
import Html.App as App
import Animation exposing (Animation)
import AnimationFrame
import Extra.Cmd exposing (noFx, updateMany)
import Task
import Color
import Diagram
import Symbol

type alias StoryEvent a =
    { t0 : Time
    , duration: Time
    , func: (Float -> a )
    }

type alias Model a =
    { animations: List (Animation a)
    , symbol : Symbol.Model
    }

type Msg
    = NoOp
    | NewEvent (StoryEvent Symbol.Msg)
    | Animate Time
    | SymbolMsg Symbol.Msg


init =
    let (sym, sfx) = Symbol.init 0 Symbol.Circle Color.red (vec2 20 20) (vec2 50 50)
        evt = StoryEvent 1000 3000 (\t -> Symbol.SetColor <| if t > 2500 then Color.green else Color.red)
        m0 = { animations = [], symbol = sym }
    in
        let (next,fx) = update (NewEvent evt) m0
        in  next ! [ Cmd.map SymbolMsg sfx, fx ]


delay: Time->Animation a ->Animation a
delay by animation =
    by
        |> Animation.interval
        |> Animation.map (always <| Animation.sample animation)
        |> Animation.continue (Animation.timeLeft animation)
           (\_ t -> Debug.log "running"
            animation
                |> Animation.run t
                |> Animation.sample
            )

update msg model =
    case msg of
        NoOp ->
            noFx model

        SymbolMsg msg ->
            let (next,fx) = Symbol.update msg model.symbol
            in ({ model | symbol = next }, Cmd.map SymbolMsg fx)


        Animate dt ->
            let m0 = { model | animations = List.map (Animation.run dt) model.animations }
                msgs = List.map (SymbolMsg << Animation.sample) model.animations
            in updateMany msgs update (noFx m0)

        NewEvent evt ->
            let anim =  delay evt.t0 (Animation.interval evt.duration |> Animation.map evt.func)
            in ({ model | animations = (model.animations)++[anim]}, startAsNeeded NoOp )


view model =
    Html.div []
        [ Html.h2 [] [Html.text "animations"]
        , Html.div [] (List.map (Html.text << toString) model.animations)
        , Svg.svg
            [ SA.width "640"
            , SA.height "480"
            , SA.viewBox "0 0 640 480"
            ]
            [ App.map (\x -> NoOp) (Diagram.viewSymbol (0,model.symbol))]
        ]


subscriptions : Model a -> Sub (Msg)
subscriptions model =
    if List.all Animation.isDone model.animations then
        Sub.none
    else
        AnimationFrame.diffs Animate

startAsNeeded msg =
    Task.perform (always msg) (always msg) <| Task.succeed msg



main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- animation : Animation Color
-- animation =
--     (2 * Time.second)
--         |> Animation.interval
--         |> Animation.map
--             (\t ->
--                 if (t < 1) then
--                     Color.green
--                 else
--                     Color.red
--             )
