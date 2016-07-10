## Trying out szabbas Animation library.

Today, I want to see things move or change on screen without any interaction.
The easiest thing I can think of, would be to fill the circles with color, 
cycling through "white, grey, green, red".

### Reading the docs

http://package.elm-lang.org/packages/szabba/elm-animations/2.0.0/Animation#Animation

As I am always trying to look for the most simple thing that could work, my eyes stick on `run` and `immediately` and `sample`. I really would like to see a small sample at this point, when I scroll way down, I see one that I at first deem to complicated, because it's in the context of `continue` or `andMap`. 

The sample there is really cool tho:

```
y : Animation Float
y =
    Animation.interval pi
    |> Animation.map cos
```

Looks like something runs up to pi and maps it to `cos` too. I am just wondering, how to start the animation and how to consume the current value. 

The `Slider`-example is where I find and understand the `Start`-Msg which seems to just create an animation and store it in the model.

Then, a second `Msg` called `Animate Time` will be called, and must be kept running with `Animation.run dt model.animation`(?)

At the same time, I could get the value of the animation with `Animation.sample model.animation` and use that to set another value (Color in this case)

### Let's try this out

Well, now Circle is a shape on a Symbol, I'll save my animation there for now:

```
type alias Model =
    { shape : Shape
    , color : Color
    , size : Vec2
    , pos : Vec2
    , animation: Animation Color
    }
```

and a new `Msg`

   `| Animate Time`

this changes `init`

```
init : Shape -> Color -> Vec2 -> Vec2 -> ( Model, Cmd Msg )
init shape color size pos =
    noFx <| Model shape color size pos animation
```

with `animation` being:

```
animation : Animation Color
animation =
    (2 * Time.second)
        |> Animation.interval
        |> Animation.map
            (\t ->
                if (t < 0.5) then
                    Color.green
                else
                    Color.red
            )
```

the new `update`-handler:

```
   Animate dt ->
            noFx
                { model
                    | animation = Animation.run dt model.animation
                    , color = Animation.sample model.animation
                }
```

This would probably do the trick, but I need to setup one more thing:

```
subscriptions : Model -> Sub Msg
subscriptions model =
    if Animation.isDone model.animation then
        Sub.none
    else
        AnimationFrame.diffs Animate
```

(that's not really documented anywhere but in the samples)

But that leads me to the point where I need to learn more about structuring and combining subscriptions, which is probably a good thing to know BEFORE wanting to do animations. 

My case: `examples\Basic.elm` has the `Program` and a field `diagram: Diagram`.
The `Diagram.Model` contains a `List Symbol`. Each one of the needs to subscribe to `AnimationFrame.diffs`. 

With almost no help from `szabba`, i get this:

```
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ( Dict.values <| Dict.map (\k v -> Sub.map (Modify k) <| Symbol.subscriptions v ) model.symbols
        )
```

which compiles. now it should start working magically. elm-reactor? nope.