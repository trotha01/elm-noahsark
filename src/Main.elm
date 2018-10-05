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
    | UpdateViewport Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( model
                |> mapArk (arkUpdate delta)
                |> mapArk (arkApplyWorld model.waterLevel)
            , Cmd.none
            )

        KeyPress dir ->
            ( model
                |> mapArk (arkMove model.waterLevel dir)
            , Cmd.none
            )

        TogglePause ->
            ( { model | pause = not model.pause }, Cmd.none )

        UpdateViewport viewport ->
            let
                halfHeight =
                    viewport.viewport.height / 2

                halfWidth =
                    viewport.viewport.width / 2
            in
            ( { model
                -- We add buffer space to waterLevel and ark prevent scrollbar
                | waterLevel = halfHeight - 25
                , ark = initArk ( halfWidth, halfHeight - 25 )
                , viewport = viewport
              }
            , Cmd.none
            )


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
            clamp -200 200 y
    in
    setY newY vec2


arkUpdate : Float -> Ark -> Ark
arkUpdate delta ark =
    { ark
        | position = displacement delta ark.position ark.velocity ark.acceleration
        , velocity =
            Vec2.add ark.velocity (Vec2.scale delta ark.acceleration)
                |> applyTerminalVelocity
    }


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
                { ark | velocity = Vec2.add (vec2 0 -100) ark.velocity }

        Down ->
            -- only allow down key when ark is above water
            if getY ark.position > waterLevel then
                ark
            else
                { ark | velocity = Vec2.add (vec2 0 100) ark.velocity }



-- VIEW


view : Model -> Document Msg
view model =
    { title = title
    , body =
        [ viewWater model
        , viewArk model
        ]
    }


viewWater : Model -> Html Msg
viewWater model =
    div
        [ style "position" "absolute"
        , style "top" (px (model.viewport.viewport.height / 2))
        , style "width" "98%"
        , style "height" (px model.waterLevel)
        , style "background-color" "lightblue"
        ]
        [ text "water" ]


px : Float -> String
px x =
    String.fromFloat x ++ "px"


viewArk : Model -> Html Msg
viewArk model =
    div
        [ style "position" "absolute"
        , style "top" (px (getY model.ark.position))
        , style "left" (px (getX model.ark.position))
        , style "width" "100px"
        , style "height" "40px"
        , style "background-color" "saddlebrown"
        , style "text-align" "center"
        ]
        [ text "ark" ]


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
            ]
