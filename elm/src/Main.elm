module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, canvas, div, form, input, label, option, select, table, td, text, tr)
import Html.Attributes exposing (checked, height, id, name, step, style, type_, value, width)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode
import Maybe
import Regex
import Svg
import Svg.Attributes



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type Scheibe
    = Scheibe Float Int String Int -- Gewicht Höhe Farbe Anzahl


scheiben : List Scheibe
scheiben =
    [ Scheibe 50 300 "palegreen" 2
    , Scheibe 25 300 "red" 0
    , Scheibe 20 300 "royalblue" 4
    , Scheibe 15 230 "palegreen" 0
    , Scheibe 10 200 "orangered" 2
    , Scheibe 5 160 "blue" 2
    , Scheibe 2.5 140 "mediumseagreen" 2
    , Scheibe 2 120 "palegreen" 0
    , Scheibe 1.25 100 "orangered" 2
    , Scheibe 0.5 80 "palegreen" 0
    ]


scheibenHöhe : Float -> Int
scheibenHöhe gewicht =
    scheiben
        |> List.filter
            (\(Scheibe gew _ _ _) ->
                gew == gewicht
            )
        |> List.head
        |> Maybe.map (\(Scheibe _ höhe _ _) -> höhe)
        |> Maybe.withDefault 0


scheibenFarbe : Float -> String
scheibenFarbe gewicht =
    scheiben
        |> List.filter
            (\(Scheibe gew _ _ _) ->
                gew == gewicht
            )
        |> List.head
        |> Maybe.map (\(Scheibe _ _ farbe _) -> farbe)
        |> Maybe.withDefault "pink"


type alias Model =
    { verfügbareScheiben : Dict Float Int
    , verschluss : String
    , stange : String
    , gewichtAnzeige : Bool
    , gewichtTotalEingabe : String
    , normiert : NormiertesModel
    }


type alias NormiertesModel =
    { eingabeIstLeer : Bool
    , eingabeIstUngültig : Bool
    , verschluss : Float
    , stange : Float
    , gewichtAnzeige : Bool
    , gewichtTotal : Float
    , gewichtZuStecken : Float
    , verwendeteScheiben : List Float
    , kilosÜbrig : Float
    }


init : Model
init =
    { verfügbareScheiben = Dict.fromList (List.map (\(Scheibe gewicht _ _ anzahl) -> ( gewicht, anzahl )) scheiben)
    , verschluss = "0"
    , stange = "20"
    , gewichtAnzeige = True
    , gewichtTotalEingabe = ""
    , normiert =
        { eingabeIstLeer = True
        , eingabeIstUngültig = True
        , verschluss = 0.0
        , stange = 20.0
        , gewichtAnzeige = True
        , gewichtTotal = 0.0
        , gewichtZuStecken = 0.0
        , verwendeteScheiben = []
        , kilosÜbrig = 0.0
        }
    }



-- UPDATE


type Msg
    = UpdateGewicht String
    | ClearGewicht
    | AnzahlScheiben Float Int
    | Verschluss String
    | Stange String
    | GewichtAnzeige Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateGewicht gewichtTotalUserEingabe ->
            let
                gewichtTotalEingabeGetrimmt =
                    if String.trim gewichtTotalUserEingabe == "" then
                        ""

                    else
                        gewichtTotalUserEingabe

                aktualisiertesModel =
                    { model | gewichtTotalEingabe = gewichtTotalEingabeGetrimmt }
            in
            -- Wir wollen das klassische HTML Formularverhalten emulieren und nicht bei jeder Eingabe sofort ein Update triggern.
            -- Die Normierung (und alle ihre Konsequenzen wie das Aktualisieren der Anzeige) findet nur statt, wenn der User ein Gewicht vollständig eingegeben hat...
            { aktualisiertesModel | normiert = normiere aktualisiertesModel }

        ClearGewicht ->
            let
                aktualisiertesModel =
                    { model | gewichtTotalEingabe = "" }
            in
            -- ... oder wenn er Zurücksetzen geklickt hat.
            { aktualisiertesModel | normiert = normiere aktualisiertesModel }

        AnzahlScheiben gewicht num ->
            { model | verfügbareScheiben = Dict.insert gewicht num model.verfügbareScheiben }

        Verschluss verschluss ->
            { model | verschluss = verschluss }

        Stange stange ->
            { model | stange = stange }

        GewichtAnzeige gewichtAnzeige ->
            { model | gewichtAnzeige = gewichtAnzeige }


normiere : Model -> NormiertesModel
normiere model =
    let
        eingabeIstGültig =
            Regex.contains (Regex.fromString "^[0-9]+([,.][0-9]?)?$" |> Maybe.withDefault Regex.never) model.gewichtTotalEingabe

        verfügbareScheiben =
            Dict.toList model.verfügbareScheiben
                |> List.map
                    (\( gewicht, anzahl ) ->
                        List.repeat (anzahl // 2) gewicht
                    )
                |> List.concat
                |> List.sortBy (\value -> -value)

        stange =
            floatAusDeutschemFormat model.stange

        gewichtTotal =
            floatAusDeutschemFormat model.gewichtTotalEingabe

        gewichtZuStecken =
            (gewichtTotal - stange) / 2

        verschluss =
            floatAusDeutschemFormat model.verschluss

        ( verwendeteScheiben, kilosÜbrig ) =
            berechneScheiben gewichtZuStecken verschluss verfügbareScheiben
    in
    { eingabeIstLeer = model.gewichtTotalEingabe == ""
    , eingabeIstUngültig = not eingabeIstGültig
    , verschluss = verschluss
    , stange = stange
    , gewichtAnzeige = model.gewichtAnzeige
    , gewichtTotal = gewichtTotal
    , gewichtZuStecken = gewichtZuStecken
    , verwendeteScheiben = verwendeteScheiben
    , kilosÜbrig = kilosÜbrig
    }


berechneScheiben : Float -> Float -> List Float -> ( List Float, Float )
berechneScheiben gewichtZuStecken verschlussGewicht verfügbareScheiben =
    let
        helfer : Float -> List Float -> List Float -> ( List Float, Float )
        helfer kilosÜbrig verwendeteScheiben verfügbareScheiben_ =
            case verfügbareScheiben_ of
                [] ->
                    ( List.reverse verwendeteScheiben, kilosÜbrig )

                scheibe :: restlicheScheiben ->
                    if kilosÜbrig - scheibe >= 0 then
                        helfer (kilosÜbrig - scheibe) (scheibe :: verwendeteScheiben) restlicheScheiben

                    else
                        helfer kilosÜbrig verwendeteScheiben restlicheScheiben
    in
    helfer (gewichtZuStecken - verschlussGewicht) [] verfügbareScheiben



-- VIEW


ausgabeLinksText : NormiertesModel -> String
ausgabeLinksText n =
    let
        gewichtTotal =
            deutschesFormat n.gewichtTotal

        stange =
            deutschesFormat n.stange

        kilosÜbrig =
            deutschesFormat n.kilosÜbrig
    in
    if n.eingabeIstLeer then
        ""

    else if n.eingabeIstUngültig then
        "Ungültiges Gewicht."

    else if n.stange > n.gewichtTotal then
        "Das angegebene Gewicht von " ++ gewichtTotal ++ "\u{2009}kg ist geringer als das Stangengewicht von " ++ stange ++ "\u{2009}kg."

    else if n.kilosÜbrig /= 0 then
        "Das angegebene Gewicht von " ++ gewichtTotal ++ "\u{2009}kg lässt sich mit den verfügbaren Scheiben nicht stecken.  Auf jeder Seite bliebe ein Rest von " ++ kilosÜbrig ++ "\u{2009}kg für den keine Scheiben vorliegen."

    else
        gewichtTotal
            ++ " = "
            ++ (List.map deutschesFormat
                    [ n.verschluss
                    , n.gewichtZuStecken - n.verschluss
                    , n.stange
                    , n.gewichtZuStecken - n.verschluss
                    , n.verschluss
                    ]
                    |> String.join " + "
               )


ausgabeRechtsText : NormiertesModel -> String
ausgabeRechtsText n =
    let
        gewichtZuStecken =
            deutschesFormat n.gewichtZuStecken

        istGleich =
            if List.length n.verwendeteScheiben > 0 || n.verschluss /= 0 then
                " = "

            else
                ""

        verschluss =
            if n.verschluss /= 0.0 then
                [ "Verschluss" ]

            else
                []
    in
    if n.eingabeIstLeer || n.eingabeIstUngültig || n.kilosÜbrig /= 0 then
        ""

    else
        gewichtZuStecken
            ++ istGleich
            ++ (List.map deutschesFormat n.verwendeteScheiben
                    ++ verschluss
                    |> String.join " + "
               )


scheibeTdHtml : Model -> Scheibe -> Html Msg
scheibeTdHtml model (Scheibe gewicht _ farbe _) =
    let
        anzahlScheibenEvent : String -> Msg
        anzahlScheibenEvent numAsStr =
            case String.toInt numAsStr of
                Just num ->
                    AnzahlScheiben gewicht num

                Nothing ->
                    AnzahlScheiben gewicht 0
    in
    td [ style "background-color" farbe ]
        [ text (deutschesFormat gewicht ++ "\u{2009}kg × ")
        , input
            [ name (deutschesFormat gewicht)
            , type_ "number"
            , step "2"
            , Html.Attributes.min "0"
            , Html.Attributes.max "10"
            , value (model.verfügbareScheiben |> Dict.get gewicht |> Maybe.withDefault 0 |> String.fromInt)
            , onInput anzahlScheibenEvent
            ]
            []
        ]


rechteckSvg : Int -> Int -> Int -> String -> Svg.Svg Msg
rechteckSvg x höhe breite farbe =
    Svg.rect
        [ Svg.Attributes.x (String.fromInt (x - breite // 2))
        , Svg.Attributes.y (String.fromInt (-höhe // 2 + 40))
        , Svg.Attributes.width (String.fromInt breite)
        , Svg.Attributes.height (String.fromInt höhe)
        , Svg.Attributes.style ("fill: " ++ farbe)
        ]
        []


visualisiereSvg : NormiertesModel -> Svg.Svg Msg
visualisiereSvg n =
    let
        scheibeListSvg index scheibe =
            let
                xPosition =
                    60 * index
            in
            [ rechteckSvg xPosition (scheibenHöhe scheibe) 40 (scheibenFarbe scheibe)
            , Svg.text_
                [ Svg.Attributes.x (String.fromInt xPosition)
                , Svg.Attributes.y "230"
                , Svg.Attributes.fontSize "30"
                , Svg.Attributes.textAnchor "middle"
                , Svg.Attributes.style "font-family: Helvetica"
                ]
                [ Svg.text (deutschesFormat scheibe) ]
            ]

        stangeListSvg =
            [ rechteckSvg 180 40 900 "gray"
            , rechteckSvg -30 100 20 "gray"
            ]

        scheibenElementeListSvg =
            n.verwendeteScheiben
                |> List.indexedMap scheibeListSvg
                |> List.concat

        verschlussListSvg =
            if n.verschluss /= 0 then
                [ rechteckSvg (List.length n.verwendeteScheiben * 60 + 40) 60 60 "blue" ]

            else
                []

        gewichtAnzeigeListSvg =
            if n.gewichtAnzeige then
                [ Svg.text_
                    [ Svg.Attributes.x "730"
                    , Svg.Attributes.y "-45"
                    , Svg.Attributes.fontSize "200"
                    , Svg.Attributes.textAnchor "end"
                    , Svg.Attributes.style "font-family: Helvetica"
                    ]
                    [ Svg.text (deutschesFormat n.gewichtTotal) ]
                ]

            else
                []
    in
    if n.eingabeIstUngültig || n.kilosÜbrig /= 0.0 then
        Svg.svg [] []

    else
        Svg.svg
            [ id "svg"
            , Svg.Attributes.viewBox "-70 -200 800 600"
            , Svg.Attributes.preserveAspectRatio "xMinYMin slice"
            , style "height" "100%"
            , style "left" "0"
            , style "position" "absolute"
            , style "top" "0"
            , style "width" "100%"
            , Svg.Attributes.version "1.1"
            ]
            (stangeListSvg ++ scheibenElementeListSvg ++ verschlussListSvg ++ gewichtAnzeigeListSvg)


view : Model -> Html Msg
view model =
    let
        alleVerfügbarenScheibenListTdHtml =
            List.map (scheibeTdHtml model) scheiben
    in
    div []
        [ form [ id "scheiben" ]
            [ table []
                [ tr []
                    (alleVerfügbarenScheibenListTdHtml
                        ++ [ td [ style "background-color" "beige" ]
                                [ select [ onInput Verschluss ]
                                    [ option [ value "0" ] [ text "0\u{2009}kg" ]
                                    , option [ value "2,5" ] [ text "2,5\u{2009}kg" ]
                                    ]
                                , text "Verschlüsse"
                                ]
                           , td [ style "background-color" "gray" ]
                                [ select [ onInput Stange ]
                                    [ option [ value "20" ] [ text "20\u{2009}kg" ]
                                    , option [ value "25" ] [ text "25\u{2009}kg" ]
                                    ]
                                , text "Stange"
                                ]
                           , td [ style "background-color" "lightblue" ]
                                [ label []
                                    [ input [ type_ "checkbox", checked model.gewichtAnzeige, onCheck GewichtAnzeige ] []
                                    , text "Gewichtanzeige"
                                    ]
                                ]
                           ]
                    )
                ]
            ]
        , form [ id "eingabe", onSubmitCapture ]
            [ input [ type_ "Text", id "kilos", value model.gewichtTotalEingabe ] []
            , text " kg"
            , button [ type_ "submit" ] [ text "Berechnen" ]
            , button [ type_ "button", onClick ClearGewicht ] [ text "Zurücksetzen" ]
            , div
                [ style "background-color" "orange"
                , style "display" "inline-block"
                , style "margin-right" "0.5em"
                ]
                [ ausgabeLinksText model.normiert |> text ]
            , div
                [ style "background-color" "orange"
                , style "display" "inline-block"
                ]
                [ ausgabeRechtsText model.normiert |> text ]
            ]
        , div
            [ style "width" "100%"
            , style "position" "relative"
            , style "user-select" "none"
            ]
            [ canvas
                [ style "visibility" "hidden"
                , style "display" "block"
                , style "width" "100%"
                , style "height" "100%"
                , width 100
                , height 54
                ]
                []
            , visualisiereSvg model.normiert
            ]
        ]


onSubmitCapture : Html.Attribute Msg



-- Wir wollen das klassische HTML Formularverhalten emulieren und nicht bei jeder Eingabe sofort ein Update triggern.


onSubmitCapture =
    let
        decoder =
            Json.Decode.map UpdateGewicht (Json.Decode.at [ "target", "0", "value" ] Json.Decode.string)
    in
    Html.Events.preventDefaultOn "submit" (Json.Decode.map (\msg -> ( msg, True )) decoder)



-- UTILITY


deutschesFormat : Float -> String
deutschesFormat gewicht =
    String.fromFloat gewicht |> String.replace "." ","


floatAusDeutschemFormat : String -> Float
floatAusDeutschemFormat deFormat =
    let
        usFormat =
            String.replace "," "." deFormat
    in
    case String.toFloat usFormat of
        Just wert ->
            wert

        Nothing ->
            0.0
