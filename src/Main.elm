module Main exposing (main)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp, onAnimationFrameDelta)
import Json.Decode as Decode
import Dict exposing (Dict)
import Array exposing (..)
import Random
import List exposing (take)
import Html exposing (time)
import Html.Events.Extra.Touch as Touch exposing(Touch)


main : Program Flags  Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODELL
type alias Model =
    { gameStatus: GameStatus
    , screenWidth: Int
    , screenHeight: Int 
    , score: Int
    , time: Float
    , player : Player
    , apple : Apple
    , keys : Keys
    , touch : Touches
    , seed : Random.Seed
    , rectangles: Rectangles
    , rectangleBuffer: Int
    }

type GameStatus 
        = Starting
        | Running
        | End

type TouchEvent
    = TouchStart
    | TouchMove
    | TouchEnd
                    
type alias Apple =
    { x : Int
    , y : Int
    , size: Int
    }

type alias Rectangles = List Rectangle

type alias Rectangle =
    { x : Int
    , y : Int
    , width: Int
    , height: Int
    , fill: String
    , velocity: Float
    }

type alias Player =
    { x : Int
    , y : Int
    , animations : Animations
    , currentSlot : String
    , maxSlotElement: Int
    , currentSlotElement : Int
    , slotChange : Float
    , slotTime : Float
    , velocity : Float
    }

type alias Animations = 
    Dict String (Array String)

type alias Keys =
    Dict String Bool

type alias Touches =
    { lastXY : (Float, Float)
    , difXY : (Float, Float)}

-- VIEW
view : Model -> Svg.Svg Msg
view { player, rectangles, screenWidth, screenHeight, score, time, apple, gameStatus, touch } = 
   svg [width (String.fromInt screenWidth)
        , height (String.fromInt screenHeight)
        , viewBox ("0 0 " ++ (String.fromInt screenWidth) ++ " " ++ (String.fromInt screenHeight))
        , Svg.Attributes.style "background: white"
        , Touch.onStart <| Touch TouchStart
        , Touch.onMove <| Touch TouchMove
        , Touch.onEnd <| Touch TouchEnd]
        [g [ width (String.fromInt screenWidth)
            , height "100"
            , viewBox ("0 0 " ++ (String.fromInt screenWidth) ++ " 100")
            , Svg.Attributes.style "background: green"
            ]
            [ viewScoreTime score time
            ]
        , g [ transform "translate(0 100)"
            , width (String.fromInt screenWidth)
            , height (String.fromInt screenHeight)
            , viewBox ("0 0 " ++ (String.fromInt screenWidth) ++ " " ++ (String.fromInt screenHeight))
            , Svg.Attributes.style "background: white"
            ]
            [ image 
                [ xlinkHref "assets/images/apple.png"
                , x (String.fromInt apple.x)
                , y (String.fromInt apple.y)
                , width (String.fromInt apple.size)
                , height (String.fromInt apple.size)]
                []
            , viewRectangles (take 20 rectangles)
            , image 
                [ xlinkHref (viewPlayerImage player)
                , x (String.fromInt player.x)
                , y (String.fromInt player.y)
                , width "64"
                , height "64"]
                []
            , viewRectangles (List.drop 20 rectangles)
            , viewMessage gameStatus
            ]
    ]
viewPlayerImage : Player -> String
viewPlayerImage player =
    let
        maybeArray = Dict.get player.currentSlot player.animations
        maybeArray1 = Maybe.withDefault (Array.fromList ["assets/images/player_01.png"]) maybeArray
        maybeElement = Array.get player.currentSlotElement maybeArray1
        element = Maybe.withDefault "assets/images/player_01.png" maybeElement
  in
    element

viewRectangles : Rectangles -> Svg msg
viewRectangles rectangles =
    g []
    (List.map viewRectangle rectangles)


viewRectangle : Rectangle -> Svg msg
viewRectangle rectangle =
    rect
        [ Svg.Attributes.x <| String.fromInt rectangle.x
        , Svg.Attributes.y <| String.fromInt rectangle.y
        , Svg.Attributes.width <| String.fromInt rectangle.width
        , Svg.Attributes.height <| String.fromInt rectangle.height
        , Svg.Attributes.fill rectangle.fill
        ]
        []
viewMessage : GameStatus -> Svg.Svg Msg
viewMessage gameStatus =
    let
        message = case gameStatus of
                    Starting ->
                        "Press any key to start"
                    Running ->
                        ""
                    End ->
                        "Game ended"
    in
    g
        [ fontSize "60px"
        , fontFamily "monospace"
        ]
       [ text_ [ x "10", y "250", textAnchor "start" ]
            [ text <| message]
        ]


viewScoreTime : Int -> Float -> Svg.Svg Msg
viewScoreTime score time =
    let
        seconds = floor(time/1000)
        timeString = if seconds >= 60 then
                        "1:00"
                     else
                        "0:" ++ if seconds < 10 then
                                    "0" ++ String.fromInt seconds
                                else
                                    String.fromInt seconds

    in
    g
        [ fontSize "40px"
        , fontFamily "monospace"
        ]
       [ text_ [ x "0", y "50", textAnchor "start" ]
            [ text <| "score:" ++ String.fromInt score ]
        , text_ [ x "500", y "50", textAnchor "end" ]
            [ text <| timeString ]
        ]
randomInt : Int -> Int -> Random.Generator Int
randomInt min max =
    Random.int min max

randomFloat : Float -> Float -> Random.Generator Float
randomFloat min max =
    Random.float min max

-- UPDATE

type alias Flags =
    Int

init : Flags -> (Model, Cmd Msg)
init seed = 
    let
        seed0 = Random.initialSeed seed
        (rectangles, seed1) = initRectangles 40 seed0 []
        (apple, seed2) = initApple seed1
    in
        ({ gameStatus = Starting
        , screenWidth = 500
        , screenHeight = 500
        , score = 0
        , time = 60000
        , player = initPlayer
        , apple = apple
        , keys = initKeys
        , touch = initTouches
        , seed = seed2
        , rectangles = rectangles
        , rectangleBuffer = 10
        }
        , Cmd.none
        )

initApple: Random.Seed -> (Apple, Random.Seed)
initApple seed =
    let
        (x, seed1) = Random.step (randomInt 64 360) seed
        (y, seed2) = Random.step (randomInt 64 240) seed1
    in
        ({ x = x
        ,  y = y
        , size = 64
        }
        , seed2
        )

initRectangles : number -> Random.Seed -> Rectangles -> ( Rectangles, Random.Seed )
initRectangles count seed list =
    if count == 0 then
        (list, seed)
    else
        let
            (newRectangle, newSeed) = initRectangle seed
            newList = newRectangle :: list
        in
            initRectangles (count-1) newSeed newList
        

initRectangle : Random.Seed -> ( Rectangle, Random.Seed )
initRectangle seed =
    let 
        (x, seed1) = Random.step (randomInt 0 490) seed
        (y, seed2) = Random.step (randomInt 0 490) seed1
        (width, seed3) = Random.step (randomInt 0 100) seed2
        (height, seed4) = Random.step (randomInt 0 100) seed3
        (r, seed5) = Random.step (randomInt 0 255) seed4
        (g, seed6) = Random.step (randomInt 0 255) seed5
        (b, seed7) = Random.step (randomInt 0 255) seed6
        (velocity, seed8) = Random.step (randomFloat 0.05 0.15) seed7
        fill="rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"
    in
        ({ x = x
        ,  y = y
        ,  width = width
        ,  height = height
        ,  fill = fill
        ,  velocity = velocity
        }, seed8)


initPlayer : Player
initPlayer = 
    { x = 100
    , y = 100
    , animations = 
        Dict.fromList
        [ ("Left", Array.fromList ["assets/images/player_14.png"
                                   ,"assets/images/player_15.png"])
        , ("Right", Array.fromList ["assets/images/player_11.png"
                                  , "assets/images/player_12.png"])
        , ("Up", Array.fromList ["assets/images/player_03.png"
                                ,"assets/images/player_04.png"])
        , ("Down", Array.fromList ["assets/images/player_01.png"
                                  ,"assets/images/player_24.png"])
        , ("Default", Array.fromList ["assets/images/player_23.png"
                                  ,"assets/images/player_23.png"])                          
        ]
    , currentSlot = "Default"
    , currentSlotElement = 0
    , maxSlotElement = 1
    , slotChange = 300
    , slotTime = 300
    , velocity = 0.1
    }

initTouches : Touches
initTouches = { lastXY = (0,0)
              , difXY  = (0,0) 
              }

initKeys:Keys
initKeys =
     Dict.fromList
        [ ("Left", False)
        , ("Right", False)
        , ("Up", False)
        , ("Down", False)
        ]

type Msg
    = OnAnimationFrame Float
    | KeyDown String
    | KeyUp String
    | Touch TouchEvent Touch.Event

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[ onKeyDown (Decode.map KeyDown keyDecoder)
             , onKeyUp (Decode.map KeyUp keyDecoder)
             , onAnimationFrameDelta OnAnimationFrame
             ]

keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string
        |> Decode.andThen keyToPlayerAction

keyToPlayerAction : String -> Decode.Decoder String
keyToPlayerAction keyString =
    case keyString of
        "ArrowUp" ->
            Decode.succeed "Up"
        "ArrowDown" ->
            Decode.succeed "Down"
        "ArrowLeft" ->
            Decode.succeed "Left"
        "ArrowRight" ->
            Decode.succeed "Right"    
        _ ->
            Decode.succeed "Any"

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            ( {model| keys = Dict.update key (\_ -> Just True) model.keys}
            , Cmd.none )
        KeyUp key ->
            ( {model| keys = Dict.update key (\_ -> Just False) model.keys}
            , Cmd.none )
        Touch touchEvent eventData ->
            ( updateOnTouch touchEvent (touchCoordinates eventData) model
            , Cmd.none )
        OnAnimationFrame timeDelta ->
            updateOnFrame model timeDelta

updateOnTouch : TouchEvent -> (Float, Float) -> Model -> Model
updateOnTouch touchEvent (x, y) model =
    let
        (lastX, lastY) = model.touch.lastXY
        (difX, difY) = case touchEvent of 
                        TouchStart ->
                            (0,0)
                        TouchMove ->
                            (lastX-x, lastY-y)
                        TouchEnd ->
                            (0,0)
        (newLastX, newLastY) = case touchEvent of 
                        TouchStart ->
                            (x,y)
                        TouchMove ->
                            (x, y)
                        TouchEnd ->
                            (0,0) 
    in
        {model| 
            touch = { lastXY=(newLastX,newLastY)               
                    , difXY=(difX,difY)
            }
        }
        

touchCoordinates : Touch.Event -> (Float, Float)
touchCoordinates event =
    List.head event.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault (0,0)


updateOnFrame : Model -> Float -> ( Model, Cmd msg )
updateOnFrame model timeDelta =
    let
        gameStatus = case model.gameStatus of
                        Starting ->
                            if Dict.get "Any" model.keys == Just True then
                                Running
                            else if model.touch.lastXY /= (0,0) then
                                Running
                            else
                                model.gameStatus
                        Running ->
                            if model.time <= 0 then
                                End
                            else
                                model.gameStatus
                        End -> 
                            End
                    

        player = if gameStatus == Running then
                     updatePlayer model.player model.keys model.touch timeDelta
                 else
                     model.player
        touching =  abs((player.x - model.apple.x)) < model.apple.size &&
                    abs((player.y - model.apple.y)) < model.apple.size
        score = if touching then
                    model.score + 1
               else
                    model.score

        (x, seed1) = if touching then
                        Random.step (randomInt 64 420) model.seed
                     else
                        (model.apple.x, model.seed)

        (y, seed2) = if touching then
                        Random.step (randomInt 64 420) seed1
                     else
                        (model.apple.x, seed1)
        apple = { x = x
                , y = y
                , size =  model.apple.size
            }

        time = if gameStatus == Running then
                    updateTime timeDelta model.time
               else 
                    model.time
    in
        ( {model| 
            gameStatus = gameStatus
            , player = player
            , apple = apple
            , seed = seed2
            , rectangles = updateRectangles model.rectangles timeDelta
            , score = score
            , time = time
        }, Cmd.none )

updatePlayer : Player -> Keys -> Touches -> Float -> Player
updatePlayer player keys touch timeDelta =
    let
        (dx, dy) = touch.difXY
        xyLength = if touch.difXY /= (0,0) then
                        sqrt (dx*dx+dy*dy)
                   else
                        0
        x = 
            if dx /= 0 then
                player.x - floor(player.velocity * timeDelta * (dx/xyLength) * 1.5)
            else if Dict.get "Right" keys == Just True then
                player.x + floor(player.velocity * timeDelta)
            else if Dict.get "Left" keys == Just True then
                player.x - floor(player.velocity * timeDelta)
            else 
                player.x
        y = 
            if dy /= 0 then
                player.y - floor(player.velocity * timeDelta * (dy/xyLength) * 1.5)
            else if Dict.get "Up" keys == Just True then
                player.y - floor(player.velocity * timeDelta)
            else if Dict.get "Down" keys == Just True then
                player.y + floor(player.velocity * timeDelta)
            else 
                player.y
        currentSlot = 
            if Dict.get "Right" keys == Just True then
                "Right"
            else if Dict.get "Left" keys == Just True then
                "Left"
            else if Dict.get "Up" keys == Just True then
                "Up"
            else if Dict.get "Down" keys == Just True then
                "Down"
            else if dx < 0 then
                "Right"
            else if dx > 0 then
                "Left"
            else if dy < 0 then
                "Down"
            else if dy > 0 then
                "Up"
            else "Default"
        currentSlotElement = 
            if player.slotTime - timeDelta < 0 then
                if player.maxSlotElement > player.currentSlotElement then
                    player.currentSlotElement + 1
                else
                    0
            else
                player.currentSlotElement
        slotTime =
            if player.slotTime > 0 then
                player.slotTime - timeDelta
            else
                player.slotChange
    in
        {player| x = x
               , y = y
               , currentSlot = currentSlot
               , currentSlotElement = currentSlotElement
               , slotTime = slotTime
               }

updateRectangles : Rectangles -> Float -> Rectangles
updateRectangles rectangles timeDelta =
    List.map (updateRectangleY timeDelta) rectangles

updateRectangleY : Float -> Rectangle -> Rectangle
updateRectangleY timeDelta rectangle  =
    let
        diff = floor(rectangle.velocity * timeDelta)
        newY = if rectangle.y + diff > 510 then
                    -20
                  else 
                    rectangle.y + diff
    in
    { rectangle | y = newY }

updateTime : Float -> Float -> Float
updateTime timeDelta time =
    Basics.max (time - timeDelta) 0
