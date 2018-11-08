module Main exposing (Ark, Bird, Circle, Direction(..), Fish, Flags, Model, Msg(..), Positioned, addBirds, addFish, applySplash, applyTerminalVelocity, arkApplyGravity, arkApplyWater, arkApplyWorld, arkMove, arkUpdate, birdGenerator, birdTop, birdsUpdate, checkCollisions, circlesCollide, displacement, fishGenerator, fishTop, fishUpdate, gravity, init, initArk, initBird, initFish, initScene, initSubViewport, initViewport, injectStyle, isOffscreen, keyDecoder, light, main, mapArk, mapBirds, mapFish, px, radFromFloat, realignArk, realignBird, realignFish, removeOldAnimals, subscriptions, title, toDirection, update, updateRain, updateTimeElapsed, view, viewArk, viewBackground, viewBirds, viewFish, viewRain, viewWater, waterTop)

import Browser exposing (..)
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Math.Vector2 as Vec2 exposing (..)
import Random as Rand exposing (..)
import String
import Task
import Time


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { points : Int
    , waterLevel : Float
    , ark : Ark
    , fish : List Fish
    , birds : List Bird
    , rainPosition : Int
    , viewport : Viewport
    , pause : Bool
    , timeElapsed : Float
    , seed : Rand.Seed
    }


type alias Ark =
    { position : Vec2
    , velocity : Vec2
    , acceleration : Vec2
    , radius : Float
    }


type alias Fish =
    { id : Float
    , position : Vec2
    , velocity : Vec2
    , radius : Float
    }


type alias Bird =
    { id : Float
    , position : Vec2
    , velocity : Vec2
    , radius : Float
    }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { points = 0
      , waterLevel = 0
      , ark = initArk ( 0, 0 )
      , fish = []
      , birds = []
      , rainPosition = 0
      , viewport = initViewport
      , pause = True
      , timeElapsed = 0
      , seed = Rand.initialSeed 0
      }
    , Task.perform UpdateViewport getViewport
    )


initArk : ( Float, Float ) -> Ark
initArk ( x, y ) =
    { position = vec2 x y
    , velocity = vec2 0 0
    , acceleration = vec2 0 0
    , radius = 50
    }


initFish : Float -> ( Float, Float ) -> Fish
initFish id ( x, y ) =
    { id = id
    , position = vec2 x y
    , velocity = vec2 -50 0
    , radius = 20
    }


initBird : Float -> ( Float, Float ) -> Bird
initBird id ( x, y ) =
    { id = id
    , position = vec2 x y
    , velocity = vec2 -50 0
    , radius = 20
    }


initViewport =
    { scene = initScene
    , viewport = initSubViewport
    }


initScene =
    { width = 0, height = 0 }


initSubViewport =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    }



-- UPDATE


type Direction
    = Up
    | Down


type Msg
    = Tick Float
    | KeyPress Direction
    | TogglePause
    | UpdateRain Time.Posix
    | UpdateViewport Viewport
    | WindowResize
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WindowResize ->
            ( model, Task.perform UpdateViewport getViewport )

        UpdateRain delta ->
            ( model |> updateRain, Cmd.none )

        Tick delta ->
            ( model
                |> updateTimeElapsed delta
                |> mapArk (arkUpdate model.waterLevel delta)
                |> mapBirds (birdsUpdate delta)
                |> mapFish (fishUpdate delta)
                |> mapArk (arkApplyWorld model.waterLevel)
                |> addBirds model.viewport delta
                |> addFish model.viewport delta
                |> checkCollisions
                |> removeOldAnimals
            , Cmd.none
            )

        KeyPress dir ->
            model
                |> mapArk (arkMove model.waterLevel dir)
                |> update (Tick 0)

        TogglePause ->
            ( { model | pause = not model.pause }, Cmd.none )

        UpdateViewport viewport ->
            let
                halfHeight =
                    viewport.scene.height / 2

                halfWidth =
                    viewport.scene.width / 2
            in
            ( { model
                -- We add buffer space to waterLevel and ark prevent scrollbar
                | waterLevel = halfHeight
                , ark = initArk ( halfWidth, halfHeight )
                , viewport = viewport
              }
            , Cmd.none
            )


updateTimeElapsed : Float -> Model -> Model
updateTimeElapsed delta model =
    { model | timeElapsed = model.timeElapsed + delta }


updateRain : Model -> Model
updateRain model =
    { model | rainPosition = modBy 5000 (model.rainPosition + 100) }


gravity : Vec2
gravity =
    vec2 0 9.8
        -- convert m/s^2 to pixels/s^2
        |> Vec2.scale 10


mapArk : (Ark -> Ark) -> Model -> Model
mapArk f model =
    { model | ark = f model.ark }


mapBirds : (List Bird -> List Bird) -> Model -> Model
mapBirds f model =
    { model | birds = f model.birds }


mapFish : (List Fish -> List Fish) -> Model -> Model
mapFish f model =
    { model | fish = f model.fish }


arkApplyWorld : Float -> Ark -> Ark
arkApplyWorld waterLevel ark =
    if getY ark.position > waterLevel then
        arkApplyWater ark

    else
        arkApplyGravity ark


arkApplyGravity : Ark -> Ark
arkApplyGravity ark =
    { ark | acceleration = gravity }


arkApplyWater : Ark -> Ark
arkApplyWater ark =
    { ark | acceleration = Vec2.scale -1 gravity }


applyTerminalVelocity : Vec2 -> Vec2
applyTerminalVelocity vec2 =
    let
        y =
            getY vec2

        newY =
            clamp -600 600 y
    in
    setY newY vec2


arkUpdate : Float -> Float -> Ark -> Ark
arkUpdate waterLevel delta ark =
    let
        newPosition =
            displacement delta ark.position ark.velocity ark.acceleration

        newVelocity =
            Vec2.add ark.velocity (Vec2.scale delta ark.acceleration)
                |> applyTerminalVelocity
                |> applySplash waterLevel ark.position newPosition
    in
    { ark
        | position = newPosition
        , velocity = newVelocity
    }


birdsUpdate : Float -> List Bird -> List Bird
birdsUpdate delta birds =
    let
        newPosition bird =
            { bird | position = displacement delta bird.position bird.velocity (vec2 0 0) }
    in
    List.map newPosition birds


fishUpdate : Float -> List Fish -> List Fish
fishUpdate delta fishList =
    let
        newPosition fish =
            { fish | position = displacement delta fish.position fish.velocity (vec2 0 0) }
    in
    List.map newPosition fishList


removeOldAnimals : Model -> Model
removeOldAnimals model =
    { model
        | fish = List.filter isOffscreen model.fish
        , birds = List.filter isOffscreen model.birds
    }


isOffscreen : Positioned a -> Bool
isOffscreen item =
    (getX item.position < 50)
        || (getX item.position > 50)
        || (getY item.position > 100)
        || (getY item.position < 100)


applySplash : Float -> Vec2 -> Vec2 -> Vec2 -> Vec2
applySplash waterLevel oldPosition newPosition velocity =
    if getY oldPosition < waterLevel && getY newPosition > waterLevel then
        Vec2.sub velocity (vec2 0 ((waterLevel - getY oldPosition) * splashScale))

    else
        velocity


splashScale =
    50


displacement : Float -> Vec2 -> Vec2 -> Vec2 -> Vec2
displacement delta position velocity acceleration =
    -- p1 = p0 + (v * t) + (1/2 * a * t^2)
    Vec2.add
        (Vec2.add position
            (Vec2.scale delta velocity)
        )
        (Vec2.scale (0.5 * delta ^ 2) acceleration)


arkMove : Float -> Direction -> Ark -> Ark
arkMove waterLevel direction ark =
    case direction of
        Up ->
            -- only allow up key when ark is underwater
            if getY ark.position < waterLevel then
                ark

            else
                { ark | velocity = Vec2.add (vec2 0 -50) ark.velocity }

        Down ->
            -- only allow down key when ark is above water
            if getY ark.position > waterLevel then
                ark

            else
                { ark | velocity = Vec2.add (vec2 0 50) ark.velocity }


realignFish : Model -> Fish -> Fish
realignFish model fish =
    { fish | position = setY (fishTop model fish) fish.position }


realignBird : Model -> Bird -> Bird
realignBird model bird =
    { bird | position = setY (birdTop model bird) bird.position }


realignArk : Model -> Ark -> Ark
realignArk model ark =
    { ark | position = setY (model.viewport.scene.height / 2) ark.position }


checkCollisions : Model -> Model
checkCollisions model =
    let
        ( newArk, newFishList, fishPoints ) =
            List.foldl
                (\fish ( ark, uneaten, points ) ->
                    if circlesCollide (realignArk model ark) (realignFish model fish) then
                        ( ark, uneaten, points + 10 )

                    else
                        ( ark, fish :: uneaten, points )
                )
                ( model.ark, [], 0 )
                model.fish

        ( newArk2, newBirdList, birdPoints ) =
            List.foldl
                (\bird ( ark, uneaten, points ) ->
                    if circlesCollide (realignArk model ark) (realignBird model bird) then
                        ( ark, uneaten, points + 10 )

                    else
                        ( ark, bird :: uneaten, points )
                )
                ( newArk, [], 0 )
                model.birds
    in
    { model
        | ark = newArk2
        , fish = newFishList
        , birds = newBirdList
        , points = model.points + fishPoints + birdPoints
    }


type alias Positioned a =
    { a | position : Vec2 }


type alias Circle a =
    Positioned { a | radius : Float }


{-| circlesCollide checks if two circles overlap
-}
circlesCollide : Circle a -> Circle b -> Bool
circlesCollide c1 c2 =
    let
        distSq =
            Vec2.distanceSquared c1.position c2.position

        radSq =
            (c1.radius + c2.radius) ^ 2
    in
    distSq < radSq



-- VIEW


view : Model -> Document Msg
view model =
    { title = title
    , body =
        if model.pause then
            [ viewGamePaused model ]

        else
            [ viewGamePlaying model ]
    }


viewGamePaused : Model -> Html Msg
viewGamePaused model =
    div []
        [ injectStyle
        , viewBackground model
        , viewWater model
        , viewArk model
        , viewRain model
        , viewBirds model
        , viewFish model
        , viewPauseScreen model
        ]


viewPauseScreen : Model -> Html Msg
viewPauseScreen model =
    div
        [ style "position" "absolute"
        , style "left" (px <| model.viewport.viewport.width / 2)
        , style "top" (px <| model.viewport.viewport.height / 2)
        , style "transform" "translate(-50%, -50%)"
        , style "border" "3px solid blue"
        , style "background" "hsla(240, 100%, 50%, 0.45)"
        , style "color" "white"
        , style "width" "600px"
        , style "height" "300px"
        , style "text-align" "center"
        , style "padding" "10px"
        , style "font-size" "2rem"
        ]
        [ h1 [] [ text "Ark" ]
        , p [] [ text "Press 'spacebar' to begin." ]
        , p [] [ text "Use 'up' and 'down' arrows to navigate." ]
        ]


viewGamePlaying : Model -> Html Msg
viewGamePlaying model =
    div []
        [ injectStyle
        , viewBackground model
        , viewWater model
        , viewArk model
        , viewRain model
        , viewBirds model
        , viewFish model
        , viewPoints model
        ]


injectStyle : Html Msg
injectStyle =
    node "style" [] [ text "body { padding: 0; margin: 0; overflow: hidden }" ]


light : Float -> Float
light timeElapsed =
    (sin <| abs (timeElapsed / 10)) * 100


viewBackground : Model -> Html Msg
viewBackground model =
    let
        brightness =
            light model.timeElapsed

        topGradient =
            -- blue
            "hsla(244, 30%, " ++ String.fromFloat brightness ++ "%, 1)"

        bottomGradient =
            -- red
            "hsla(0, 30%, " ++ String.fromFloat brightness ++ "%, 1)"

        gradients =
            topGradient ++ ", " ++ bottomGradient
    in
    div
        [ style "background-image" ("linear-gradient(" ++ gradients ++ ")")
        , style "position" "absolute"
        , style "width" (px model.viewport.viewport.width)
        , style "height" (px model.viewport.viewport.height)
        ]
        []


viewRain : Model -> Html Msg
viewRain model =
    let
        isDaytime =
            light model.timeElapsed > 70

        background =
            if isDaytime then
                style "" ""

            else if model.rainPosition == 100 || model.rainPosition == 800 then
                style "background-color" "rgba(255, 255, 255, 0.9)"

            else
                style "background" "url(imgs/rain.png)"

        height =
            clamp 0
                model.viewport.scene.height
                (model.viewport.scene.height - getY model.ark.position)
    in
    div
        [ background
        , style "background-position"
            (px (toFloat model.rainPosition)
                ++ " "
                ++ px (toFloat model.rainPosition)
            )
        , style "position" "absolute"
        , style "top" "0px"
        , style "width" (px model.viewport.viewport.width)
        , style "height" (px height)
        ]
        []


viewPoints : Model -> Html Msg
viewPoints model =
    div
        [ style "position" "fixed"
        , style "top" "10px"
        , style "right" "10px"
        , style "background-color" "hsla(0, 0%, 100%, 0.61)"
        , style "height" "50px"
        , style "width" "100px"
        , style "padding" "10px"
        , style "font-size" "2rem"
        ]
        [ text <| String.fromInt model.points ]


fishTop : Model -> Fish -> Float
fishTop model fish =
    let
        waterLevel =
            model.viewport.viewport.height - getY model.ark.position
    in
    getY fish.position + waterLevel


viewFish : Model -> Html Msg
viewFish model =
    div []
        (List.map
            (\fish ->
                div
                    [ style "position" "absolute"
                    , style "top" (px <| fishTop model fish)
                    , style "left" (px <| getX fish.position)
                    ]
                    [ img [ src "imgs/fish.png", width 50, height 50 ] [] ]
            )
            model.fish
        )


birdTop : Model -> Bird -> Float
birdTop model bird =
    let
        waterLevel =
            model.viewport.viewport.height - getY model.ark.position
    in
    getY bird.position - (model.viewport.scene.height - waterLevel)


viewBirds : Model -> Html Msg
viewBirds model =
    div []
        (List.map
            (\bird ->
                div
                    [ style "position" "absolute"
                    , style "top" (px <| birdTop model bird)
                    , style "left" (px <| getX bird.position)
                    ]
                    [ img [ src "imgs/bird.png", width 50, height 50 ] [] ]
            )
            model.birds
        )


waterTop : Model -> Float
waterTop model =
    clamp 0
        model.viewport.scene.height
        (model.viewport.viewport.height - getY model.ark.position)


viewWater : Model -> Html Msg
viewWater model =
    let
        halfHeight =
            model.viewport.viewport.height / 2

        top =
            waterTop model

        height =
            clamp 0
                model.viewport.scene.height
                (model.viewport.viewport.height - top)

        brightness =
            79 - (halfHeight + getY model.ark.position) / 40
    in
    div
        [ style "position" "absolute"
        , style "top" (px top)
        , style "width" "100%"
        , style "height" (px height)
        , style "background-color" ("hsla(195, 53%, " ++ String.fromFloat brightness ++ "% , 0.8)")
        ]
        []


px : Float -> String
px x =
    String.fromFloat x ++ "px"


viewArk : Model -> Html Msg
viewArk model =
    let
        arkTilt =
            atan2 (getY model.ark.velocity) 300

        centerScreen =
            model.viewport.scene.height / 2
    in
    div
        [ style "position" "absolute"
        , style "top" (px centerScreen)
        , style "left" (px (getX model.ark.position))
        , style "width" "100px"
        , style "height" "40px"
        , style "background-color" "saddlebrown"
        , style "text-align" "center"
        , style "transform" ("rotate(" ++ radFromFloat arkTilt ++ ")")
        ]
        []


radFromFloat : Float -> String
radFromFloat r =
    String.fromFloat r ++ "rad"


title : String
title =
    "Noah's Ark"


addBirds : Viewport -> Float -> Model -> Model
addBirds viewport delta model =
    let
        ( newBirdList, seed ) =
            case Rand.step (birdGenerator viewport model.timeElapsed) model.seed of
                ( Nothing, newSeed ) ->
                    ( model.birds, newSeed )

                ( Just newBird, newSeed ) ->
                    ( newBird :: model.birds, newSeed )
    in
    { model | birds = newBirdList, seed = seed }


addFish : Viewport -> Float -> Model -> Model
addFish viewport delta model =
    let
        ( newFishList, seed ) =
            case Rand.step (fishGenerator viewport model.timeElapsed) model.seed of
                ( Nothing, newSeed ) ->
                    ( model.fish, newSeed )

                ( Just newFish, newSeed ) ->
                    ( newFish :: model.fish, newSeed )
    in
    { model | fish = newFishList, seed = seed }


birdGenerator : Viewport -> Float -> Generator (Maybe Bird)
birdGenerator viewport id =
    Rand.map2
        (\i y ->
            if i > 1 then
                Nothing

            else
                Just (initBird id ( viewport.scene.width, y ))
        )
        (Rand.int 0 100)
        (Rand.float 20 (viewport.scene.height - 20))


fishGenerator : Viewport -> Float -> Generator (Maybe Fish)
fishGenerator viewport id =
    Rand.map2
        (\i y ->
            if i > 1 then
                Nothing

            else
                Just (initFish id ( viewport.scene.width + 20, y ))
        )
        (Rand.int 0 100)
        (Rand.float 20 (viewport.scene.height - 20))



-- SUBSCRIPTIONS


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowUp" ->
            KeyPress Up

        "ArrowDown" ->
            KeyPress Down

        " " ->
            TogglePause

        _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.pause then
        Sub.batch
            [ onKeyDown keyDecoder
            , Browser.Events.onResize (\_ _ -> WindowResize)
            ]

    else
        Sub.batch
            [ onAnimationFrameDelta (\delta -> Tick (delta / 1000)) -- convert to seconds
            , onKeyDown keyDecoder
            , Time.every 100 UpdateRain
            , Browser.Events.onResize (\_ _ -> WindowResize)
            ]
