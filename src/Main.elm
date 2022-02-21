port module Main exposing (Model, main, update, view)

import Browser
import Date exposing (Date, Unit(..))
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Katakana
import List.Extra as List
import Set exposing (Set)
import Task
import Time exposing (Month(..))


port copy : String -> Cmd msg



--- MODEL ---


type alias Model =
    { nextGuess : String
    , guesses : List String
    , word : String
    , wordDoesNotExist : Bool
    }


type Msg
    = AddChar Char
    | DeleteChar
    | EnterGuess
    | Share
    | GetWordForToday Date


init : () -> ( Model, Cmd Msg )
init _ =
    ( { guesses = []
      , nextGuess = ""
      , word = ""
      , wordDoesNotExist = False
      }
    , Date.today |> Task.perform GetWordForToday
    )



--- VIEW ---


view : Model -> Html Msg
view model =
    Html.div [ Attrs.class "p-8 flex flex-col gap-8 items-center" ]
        [ Html.h1 [ Attrs.class "font-['Kashima'] text-4xl" ]
            [ Html.text "Katakana Wordle! 👺" ]
        , viewWordle
            { word = model.word
            , guesses = model.guesses
            , nextGuess = model.nextGuess
            , wordDoesNotExist = model.wordDoesNotExist
            }
        , viewWin model.word model.guesses
        , viewKeyboard model.word model.guesses
        ]


viewWin : String -> List String -> Html Msg
viewWin word guesses =
    case gameState word guesses of
        Won ->
            Html.div
                [ Attrs.class "text-xl text-lime-600" ]
                [ Html.text "YOU WON! 🎉"
                , Html.button
                    [ Attrs.class "p-2 min-w-[2rem] ml-2 text-center cursor-pointer text-xl border border-gray-500 text-gray-500"
                    , Events.onClick Share
                    ]
                    [ Html.text "SHARE! (COPY TO 📋)" ]
                ]

        Lost ->
            Html.div
                [ Attrs.class "text-xl text-orange-600" ]
                [ Html.text <| "YOU LOST! 😭 " ++ "The answer was: " ++ word ]

        Playing ->
            Html.text ""


type Key
    = CharKey Char
    | DeleteKey
    | EnterGuessKey


viewKeyboard : String -> List String -> Html Msg
viewKeyboard word guesses =
    Html.div
        [ Attrs.class "flex flex-col gap-2 justify-stretch w-min" ]
        (List.map (viewKeyboardRow word guesses) keyboard)


viewKeyboardRow : String -> List String -> List Key -> Html Msg
viewKeyboardRow word guesses keys =
    Html.div
        [ Attrs.class "flex flex-row gap-2 justify-center" ]
        (List.map (viewKey word guesses) keys)


viewKey : String -> List String -> Key -> Html Msg
viewKey word guesses key =
    let
        ( msg, text, maybeClass ) =
            case key of
                CharKey char ->
                    ( AddChar char
                    , String.fromChar char
                    , scoreKeyboardChar word guesses char
                        |> Maybe.map gradeClass
                    )

                DeleteKey ->
                    ( DeleteChar, "⬅", Nothing )

                EnterGuessKey ->
                    ( EnterGuess, "ENTER", Nothing )
    in
    Html.div
        [ Events.onClick msg
        , Attrs.class "p-2 min-w-[2rem] text-center cursor-pointer text-xl border border-gray-500 text-gray-500"
        , maybeClass
            |> Maybe.withDefault ""
            |> Attrs.class
        ]
        [ Html.text text ]


toCharKeys : String -> List Key
toCharKeys keys =
    keys
        |> String.toList
        |> List.map CharKey


keyboard : List (List Key)
keyboard =
    [ toCharKeys "ァアィイゥウェエォ"
    , toCharKeys "オヮワカガキギクグ"
    , toCharKeys "ケゲコゴサザシジス"
    , toCharKeys "ズセゼソゾタダチヂ"
    , toCharKeys "ッツヅテデトドナニ"
    , toCharKeys "ヌネンノハバパヒビ"
    , toCharKeys "ピフブプヘベペホボ"
    , toCharKeys "ポマミームメモャヤ"
    , toCharKeys "ュユョヨラリルレロ"
    , [ EnterGuessKey, DeleteKey ]
    ]


wordSize : Int
wordSize =
    5


maxGuesses : Int
maxGuesses =
    10


viewWordle : Model -> Html msg
viewWordle { word, guesses, nextGuess, wordDoesNotExist } =
    let
        scoredGuesses : List (List ( Char, Grade ))
        scoredGuesses =
            scoreGuesses word guesses

        unusedGuesses : Int
        unusedGuesses =
            max 0 <|
                maxGuesses
                    - List.length guesses
                    - (if String.isEmpty nextGuess then
                        0

                       else
                        1
                      )
    in
    Html.div [ Attrs.class "flex flex-col gap-2" ]
        (List.map viewScoredGuess scoredGuesses
            ++ (if String.isEmpty nextGuess then
                    []

                else
                    [ viewNextGuess nextGuess wordDoesNotExist ]
               )
            ++ List.repeat unusedGuesses emptyRow
        )


viewNextGuess : String -> Bool -> Html msg
viewNextGuess guess wordDoesNotExist =
    let
        sanitizedGuess : String
        sanitizedGuess =
            String.left wordSize guess

        padding : Int
        padding =
            wordSize - String.length sanitizedGuess

        className : String
        className =
            if wordDoesNotExist then
                "shake " ++ rowClasses

            else
                rowClasses
    in
    Html.div [ Attrs.class className ]
        ((sanitizedGuess
            |> String.toList
            |> List.map viewNextGuessChar
         )
            ++ List.repeat padding emptyCell
        )


viewScoredGuess : List ( Char, Grade ) -> Html msg
viewScoredGuess chars =
    Html.div [ Attrs.class rowClasses ] (List.map viewScoredChar chars)


emptyRow : Html msg
emptyRow =
    Html.div [ Attrs.class rowClasses ] (List.repeat wordSize emptyCell)


rowClasses : String
rowClasses =
    "gap-2 flex flex-row"


viewScoredChar : ( Char, Grade ) -> Html msg
viewScoredChar ( char, grade ) =
    Html.div
        [ Attrs.class cellClasses
        , Attrs.class "text-white"
        , Attrs.class <| gradeClass grade
        ]
        [ Html.text <| String.fromChar char ]


gradeClass : Grade -> String
gradeClass grade =
    case grade of
        Miss ->
            "bg-gray-400"

        PresentElsewhere ->
            "bg-yellow-500"

        Hit ->
            "bg-lime-500"


emptyCell : Html msg
emptyCell =
    Html.div [ Attrs.class cellClasses ] []


viewNextGuessChar : Char -> Html msg
viewNextGuessChar char =
    Html.div [ Attrs.class cellClasses ] [ Html.text <| String.fromChar char ]


cellClasses : String
cellClasses =
    "w-[58px] h-[58px] border border-gray-400 text-gray-600 text-xl flex items-center justify-center"


scoreGuesses : String -> List String -> List (List ( Char, Grade ))
scoreGuesses word guesses =
    guesses
        |> List.map
            (String.left wordSize
                >> String.toUpper
                >> String.toList
                >> scoreGuess word
            )


scoreGuess : String -> List Char -> List ( Char, Grade )
scoreGuess word guessChars =
    let
        wordChars : List Char
        wordChars =
            String.toList word

        wordSet : Set Char
        wordSet =
            Set.fromList wordChars
    in
    List.map2
        (\correctChar guessChar ->
            ( guessChar
            , if correctChar == guessChar then
                Hit

              else if Set.member guessChar wordSet then
                PresentElsewhere

              else
                Miss
            )
        )
        wordChars
        guessChars


{-| Nothing == not guessed yet, white
-}
scoreKeyboardChar : String -> List String -> Char -> Maybe Grade
scoreKeyboardChar word guesses char =
    let
        guessedChars : Set Char
        guessedChars =
            -- grey unless proven otherwise
            guesses
                |> List.concatMap String.toList
                |> Set.fromList

        correctChars : Set Char
        correctChars =
            word
                |> String.toList
                |> Set.fromList

        hitElsewhereChars : Set Char
        hitElsewhereChars =
            scoreGuesses word guesses
                |> List.concat
                |> List.filter (\( _, grade ) -> grade == PresentElsewhere)
                |> List.map Tuple.first
                |> Set.fromList

        hitChars : Set Char
        hitChars =
            -- green
            Set.intersect
                guessedChars
                correctChars
    in
    if Set.member char hitChars then
        Just Hit

    else if Set.member char hitElsewhereChars then
        Just PresentElsewhere

    else if Set.member char guessedChars then
        Just Miss

    else
        Nothing


type Grade
    = Miss -- grey
    | PresentElsewhere -- yellow
    | Hit -- green


type GameState
    = Won
    | Lost
    | Playing


gameState : String -> List String -> GameState
gameState word guesses =
    if List.member word guesses then
        Won

    else if List.length guesses >= maxGuesses then
        Lost

    else
        Playing



--- UDPATE ---


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GetWordForToday today ->
            let
                firstDay : Date
                firstDay =
                    Date.fromCalendarDate 2022 Feb 20

                diffDays : Int
                diffDays =
                    Date.diff Days firstDay today
            in
            ( { model
                | word =
                    -- Make sure that we never run out of words!
                    List.cycle (diffDays + 1) Katakana.words
                        |> List.getAt diffDays
                        |> Maybe.withDefault ""
              }
            , Cmd.none
            )

        AddChar char ->
            ( if gameState model.word model.guesses /= Playing then
                model

              else
                { model
                    | nextGuess =
                        (model.nextGuess ++ String.fromChar char)
                            |> String.left wordSize
                }
            , Cmd.none
            )

        DeleteChar ->
            ( { model
                | nextGuess =
                    model.nextGuess
                        |> String.left (String.length model.nextGuess - 1)
                , wordDoesNotExist = False
              }
            , Cmd.none
            )

        EnterGuess ->
            ( if String.length model.nextGuess /= wordSize then
                model

              else if List.length model.guesses >= maxGuesses then
                model

              else if List.notMember model.nextGuess Katakana.words then
                { model | wordDoesNotExist = True }

              else
                { model
                    | nextGuess = ""
                    , guesses = model.guesses ++ [ model.nextGuess ]
                }
            , Cmd.none
            )

        Share ->
            let
                stateString : String
                stateString =
                    model.guesses
                        |> List.map
                            (String.toList
                                >> List.map (scoreKeyboardChar model.word model.guesses >> gradeToString)
                                >> String.join ""
                            )
                        |> String.join "\n"
            in
            ( model, copy <| stateString ++ "\n\n #KatakanaWordle \nhttps://katakana-wordle.netlify.app/" )


gradeToString : Maybe Grade -> String
gradeToString grade =
    case grade of
        Nothing ->
            "⬜️"

        Just Miss ->
            "⬜️"

        Just PresentElsewhere ->
            "🟨"

        Just Hit ->
            "🟩"



--- MAIN ---


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
