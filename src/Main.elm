module Main exposing (..)

import Browser exposing (..)
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Math.Vector2 as Vec2 exposing (..)
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
    { waterLevel : Float
    , ark : Ark
    , rainPosition : Int
    , viewport : Viewport
    , pause : Bool
    }


type alias Ark =
    { position : Vec2
    , velocity : Vec2
    , acceleration : Vec2
    }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { waterLevel = 0
      , ark = initArk ( 0, 0 )
      , rainPosition = 0
      , viewport = initViewport
      , pause = True
      }
    , Task.perform UpdateViewport getViewport
    )


initArk : ( Float, Float ) -> Ark
initArk ( x, y ) =
    { position = vec2 x y
    , velocity = vec2 0 0
    , acceleration = vec2 0 0
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateRain delta ->
            ( model |> updateRain, Cmd.none )

        Tick delta ->
            ( model
                |> mapArk (arkUpdate model.waterLevel delta)
                |> mapArk (arkApplyWorld model.waterLevel)
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


applySplash : Float -> Vec2 -> Vec2 -> Vec2 -> Vec2
applySplash waterLevel oldPosition newPosition velocity =
    if getY oldPosition < waterLevel && getY newPosition > waterLevel then
        Vec2.sub velocity (vec2 0 50)
    else
        velocity


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



-- VIEW


view : Model -> Document Msg
view model =
    { title = title
    , body =
        [ injectStyle
        , viewBackground model
        , viewWater model
        , viewArk model
        , viewRain model
        ]
    }


injectStyle : Html Msg
injectStyle =
    node "style" [] [ text "body { padding: 0; margin: 0; overflow: hidden }" ]


viewBackground : Model -> Html Msg
viewBackground model =
    div
        [ style "background-color" "black"
        , style "position" "absolute"
        , style "width" (px model.viewport.viewport.width)
        , style "height" (px model.viewport.viewport.height)
        ]
        []


viewRain : Model -> Html Msg
viewRain model =
    let
        background =
            if model.rainPosition == 100 || model.rainPosition == 800 then
                style "background-color" "rgba(255, 255, 255, 0.9)"
            else
                style "background" "url(imgs/rain.png)"
    in
    div
        [ style "height" "100%"
        , background
        , style "background-position"
            (px (toFloat model.rainPosition)
                ++ " "
                ++ px (toFloat model.rainPosition)
            )
        , style "position" "absolute"
        , style "width" (px model.viewport.viewport.width)
        , style "height" (px model.viewport.viewport.height)
        ]
        []


viewWater : Model -> Html Msg
viewWater model =
    let
        halfHeight =
            model.viewport.viewport.height / 2

        top =
            clamp 0
                model.viewport.scene.height
                ((2 * halfHeight) - getY model.ark.position)

        height =
            clamp 0
                model.viewport.scene.height
                (model.viewport.viewport.height - top)
    in
    div
        [ style "position" "absolute"
        , style "top" (px top)
        , style "width" "100%"
        , style "height" (px height)
        , style "background-color" "lightblue"
        ]
        [ text "water" ]


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
        [ text "ark" ]


radFromFloat : Float -> String
radFromFloat r =
    String.fromFloat r ++ "rad"


title : String
title =
    "Noah's Ark"



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

        _ ->
            TogglePause


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.pause then
        onKeyDown keyDecoder
    else
        Sub.batch
            [ onAnimationFrameDelta (\delta -> Tick (delta / 1000)) -- convert to seconds
            , onKeyDown keyDecoder
            , Time.every 100 UpdateRain
            ]
