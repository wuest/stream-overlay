module Main exposing (main, decodeAlert, alertDecoder)

import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import WebSocket
import Json.Decode                   as Json
import Time                          as Time

type alias Alert = { userHandle : String
                   , eventType : String
                   , amount : Float
                   , message : String
                   }

type alias Model = { activeAlert : Maybe Alert
                   , alertStatus : Maybe AnimationStatus
                   , alertQueue : List Alert
                   , sockUrl : String
                   }

type Msg = Receive String
         | Tick Time.Time

type AnimationStatus = FadeIn Time.Time
                     | Hold Time.Time
                     | FadeOut Time.Time

type alias Flags = { webPort : String }

-- Decoding data

alertDecoder : Json.Decoder Alert
alertDecoder = Json.map4 Alert ( Json.at [ "userHandle" ] Json.string )
                               ( Json.at [ "eventType" ]  Json.string )
                               ( Json.at [ "amount" ]     Json.float  )
                               ( Json.at [ "message" ]    Json.string )

decodeAlert : String -> Result String Alert
decodeAlert = Json.decodeString alertDecoder

processAlert : Model -> String -> List Alert
processAlert model msg =
    let alert = decodeAlert msg
        alerts = (.alertQueue model) in
        case alert of
            Err _ -> alerts
            Ok  a -> alerts ++ [a]

-- Display data

showActiveAlert alert = case alert of
    Just a  -> [ span [ class "donorName" ] [ text <| .userHandle a ]
               , span [] [ text <| " donated " ]
               , span [ class "amount" ] [ text (dollarAmount <| .amount a) ]
               , br [] []
               , span [ class "donationMessage" ] [ text <| .message a ]
               ]
    Nothing -> [ span [] [text "test"]]

dollarAmount : Float -> String
dollarAmount f = let cents = toString <| round <| f * 100
                 in "$" ++ (String.dropRight 2 cents) ++ "." ++
                           (String.dropLeft <| String.length cents - 2) cents

-- Handle timed events

processTick : Time.Time -> Model -> Model
processTick time model = case .alertStatus model of
    Nothing -> setActiveAlert model time
    Just a -> case a of
        FadeIn t -> if time > t
            then { model | alertStatus = Just (Hold <| time + 10 * Time.second) }
            else model
        Hold t -> if time > t
            then { model | alertStatus = Just (FadeOut <| time + 1 * Time.second) }
            else model
        FadeOut t -> if time > t
            then { model | alertStatus = Nothing }
            else model

setActiveAlert : Model -> Time.Time -> Model
setActiveAlert model time =
    let nextAlert = List.head <| .alertQueue model
        nextQueue = List.drop 1 <| .alertQueue model
        nextModel = { model | activeAlert = nextAlert , alertQueue = nextQueue }
    in
        case nextAlert of
            Nothing -> { nextModel | alertStatus = Nothing }
            Just _  -> { nextModel | alertStatus = Just (FadeIn <| time + 1 * Time.second) }

setAnimation model = case .alertStatus model of
    Nothing -> classList [ ("invisible", True) ]
    Just (FadeIn _) -> classList [ ("fadein", True) ]
    Just (Hold _) -> classList [ ("fadein", True) ]
    Just (FadeOut _) -> classList [ ("fadeout", True) ]

-- Main logic

init : Flags -> (Model, Cmd Msg)
init flags = ( { activeAlert = Nothing
               , alertStatus = Nothing
               , alertQueue = []
               , sockUrl = "ws://localhost:" ++ .webPort flags
               }
             , Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ div [ id "notification", setAnimation model ]
          (showActiveAlert <| .activeAlert model)
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Receive msg -> ({model | alertQueue = processAlert model msg }) ! []
    Tick t -> processTick t model ! []

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [ WebSocket.listen (.sockUrl model) Receive
                                , Time.every Time.second Tick
                                ]

main : Program Flags Model Msg
main =
  programWithFlags
    { init          = init
    , update        = update
    , view          = view
    , subscriptions = subscriptions
    }

