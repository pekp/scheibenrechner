module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, br, button, canvas, div, form, input, label, option, select, table, td, text, tr)
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
    = Scheibe String Int String Int -- Gewicht Höhe Farbe Anzahl


scheiben =
    [ Scheibe "50" 300 "palegreen" 2
    , Scheibe "25" 300 "red" 0
    , Scheibe "20" 300 "royalblue" 4
    , Scheibe "15" 230 "palegreen" 0
    , Scheibe "10" 200 "orangered" 2
    , Scheibe "5" 160 "blue" 2
    , Scheibe "2,5" 140 "mediumseagreen" 2
    , Scheibe "2" 120 "palegreen" 0
    , Scheibe "1,25" 100 "orangered" 2
    , Scheibe "0,5" 80 "palegreen" 0
    ]


type alias Model =
    { scheiben : Dict String Int
    , verschluss : String
    , stange : String
    , gewichtAnzeige : Bool
    , gewichtTotalEingabe : String
    }


type alias NormiertesModel =
    { eingabeLeer : Bool
    , gültigeEingabe : Bool
    , scheiben : List Float
    , verschluss : Float
    , stange : Float
    , gewichtAnzeige : Bool
    , gewichtTotal : Float
    , gewichtZuStecken : Float
    }


normiere : Model -> NormiertesModel
normiere model =
    let
        stange =
            floatAusDeutschemFormat model.stange

        gewichtTotal =
            floatAusDeutschemFormat model.gewichtTotalEingabe

        gewichtZuStecken =
            (gewichtTotal - stange) / 2
    in
    { eingabeLeer = model.gewichtTotalEingabe == ""
    , gültigeEingabe = gewichtIstGültig model.gewichtTotalEingabe
    , scheiben = scheibenList model.scheiben
    , verschluss = floatAusDeutschemFormat model.verschluss
    , stange = stange
    , gewichtAnzeige = model.gewichtAnzeige
    , gewichtTotal = gewichtTotal
    , gewichtZuStecken = gewichtZuStecken
    }


init : Model
init =
    { scheiben = Dict.fromList (List.map (\(Scheibe gewicht _ _ anzahl) -> ( gewicht, anzahl )) scheiben)
    , verschluss = "0"
    , stange = "20"
    , gewichtAnzeige = True
    , gewichtTotalEingabe = ""
    }



-- UPDATE


type Msg
    = UpdateGewicht String
    | ClearGewicht
    | Scheiben String Int
    | Verschluss String
    | Stange String
    | GewichtAnzeige Bool


gewichtIstGültig : String -> Bool
gewichtIstGültig gewicht =
    let
        gültigesGewicht =
            Regex.fromString "^[0-9]+([,.][0-9]?)?$"
                |> Maybe.withDefault Regex.never
    in
    Regex.contains gültigesGewicht gewicht


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateGewicht aktuellesGewicht ->
            { model
                | gewichtTotalEingabe =
                    if String.trim aktuellesGewicht == "" then
                        ""

                    else
                        aktuellesGewicht
            }

        ClearGewicht ->
            { model | gewichtTotalEingabe = "" }

        Scheiben gewicht num ->
            { model | scheiben = Dict.insert gewicht num model.scheiben }

        Verschluss verschluss ->
            { model | verschluss = verschluss }

        Stange stange ->
            { model | stange = stange }

        GewichtAnzeige gewichtAnzeige ->
            { model | gewichtAnzeige = gewichtAnzeige }


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


scheibenList : Dict String Int -> List Float
scheibenList scheibenDict =
    Dict.toList scheibenDict
        |> List.map
            (\( gewicht, anzahl ) ->
                List.repeat (anzahl // 2) (floatAusDeutschemFormat gewicht)
            )
        |> List.concat
        |> List.sortBy (\value -> -value)


stringFromList : List Float -> String
stringFromList list =
    let
        stringList =
            List.map String.fromFloat list

        joined =
            String.join ", " stringList
    in
    "[" ++ joined ++ "]"


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


scheibenEvent : String -> String -> Msg
scheibenEvent gewicht numAsStr =
    case String.toInt numAsStr of
        Just num ->
            Scheiben gewicht num

        Nothing ->
            Scheiben gewicht 0


lookupAsString : Dict String Int -> String -> String
lookupAsString dict key =
    dict
        |> Dict.get key
        |> Maybe.withDefault 0
        |> String.fromInt


tdScheibe :
    Model
    -> Scheibe
    -> Html Msg -- FIXME: Hier nicht lieber NormiertesModel???
tdScheibe model (Scheibe gewicht _ farbe _) =
    td [ style "background-color" farbe ]
        [ text (gewicht ++ "\u{2009}kg × ")
        , input
            [ name gewicht
            , type_ "number"
            , step "2"
            , Html.Attributes.min "0"
            , Html.Attributes.max "10"
            , value (lookupAsString model.scheiben gewicht)
            , onInput (scheibenEvent gewicht)
            ]
            []
        ]


ausgabeLinksText : NormiertesModel -> String
ausgabeLinksText n =
    let
        gewichtTotal =
            deutschesFormat n.gewichtTotal

        stange =
            deutschesFormat n.stange

        ( _, kilosÜbrigNum ) =
            berechneScheiben n.gewichtZuStecken n.verschluss n.scheiben

        kilosÜbrigS =
            deutschesFormat kilosÜbrigNum
    in
    if n.eingabeLeer then
        ""

    else if n.gültigeEingabe == False then
        "Ungültiges Gewicht."

    else if n.stange > n.gewichtTotal then
        "Das angegebene Gewicht von " ++ gewichtTotal ++ "\u{2009}kg ist geringer als das Stangengewicht von " ++ stange ++ "\u{2009}kg."

    else if kilosÜbrigNum /= 0 then
        "Das angegebene Gewicht von " ++ gewichtTotal ++ "\u{2009}kg lässt sich mit den verfügbaren Scheiben nicht stecken.  Auf jeder Seite bliebe ein Rest von " ++ kilosÜbrigS ++ "\u{2009}kg für den keine Scheiben vorliegen."

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

        ( verwendeteScheiben, kilosÜbrig ) =
            berechneScheiben n.gewichtZuStecken n.verschluss n.scheiben

        istGleich =
            if List.length verwendeteScheiben > 0 || n.verschluss /= 0 then
                " = "

            else
                ""

        verschluss =
            if n.verschluss /= 0.0 then
                " + Verschluss"

            else
                ""
    in
    if n.eingabeLeer || n.gültigeEingabe == False || kilosÜbrig /= 0 then
        ""

    else
        gewichtZuStecken ++ istGleich ++ (List.map deutschesFormat verwendeteScheiben |> String.join " + ") ++ verschluss


stringFromBool : Bool -> String
stringFromBool b =
    if b == True then
        "True"

    else
        "False"


rechteck : Int -> Int -> Int -> String -> Svg.Svg Msg
rechteck x höhe breite farbe =
    Svg.rect
        [ Svg.Attributes.x (String.fromInt (x - breite // 2))
        , Svg.Attributes.y (String.fromInt (-höhe // 2 + 40))
        , Svg.Attributes.width (String.fromInt breite)
        , Svg.Attributes.height (String.fromInt höhe)
        , Svg.Attributes.style ("fill: " ++ farbe)
        ]
        []


scheibenHöhe : Float -> Int
scheibenHöhe gewicht =
    scheiben
        |> List.filter
            (\(Scheibe gewichtStr _ _ _) ->
                floatAusDeutschemFormat gewichtStr == gewicht
            )
        |> List.head
        |> Maybe.map (\(Scheibe _ höhe _ _) -> höhe)
        |> Maybe.withDefault 0


scheibenFarbe : Float -> String
scheibenFarbe gewicht =
    scheiben
        |> List.filter
            (\(Scheibe gewichtStr _ _ _) ->
                floatAusDeutschemFormat gewichtStr == gewicht
            )
        |> List.head
        |> Maybe.map (\(Scheibe _ _ farbe _) -> farbe)
        |> Maybe.withDefault "pink"


scheibenBeschriftung : Int -> Float -> Svg.Svg Msg
scheibenBeschriftung x gewicht =
    Svg.text_
        [ Svg.Attributes.x (String.fromInt x)
        , Svg.Attributes.y "230"
        , Svg.Attributes.fontSize "30"
        , Svg.Attributes.textAnchor "middle"
        , Svg.Attributes.style "font-family: Helvetica"
        ]
        [ Svg.text (deutschesFormat gewicht) ]


anzeige : Int -> Int -> Float -> Svg.Svg Msg
anzeige x y gewicht =
    Svg.text_
        [ Svg.Attributes.x (String.fromInt x)
        , Svg.Attributes.y (String.fromInt y)
        , Svg.Attributes.fontSize "200"
        , Svg.Attributes.textAnchor "end"
        , Svg.Attributes.style "font-family: Helvetica"
        ]
        [ Svg.text (deutschesFormat gewicht) ]


zeichneStange : NormiertesModel -> Svg.Svg Msg
zeichneStange n =
    let
        ( verwendeteScheiben, _ ) =
            berechneScheiben n.gewichtZuStecken n.verschluss n.scheiben

        zeichneScheibe index scheibe =
            let
                xPosition =
                    60 * index

                höhe =
                    scheibenHöhe scheibe

                farbe =
                    scheibenFarbe scheibe
            in
            [ rechteck xPosition höhe 40 farbe
            , scheibenBeschriftung xPosition scheibe
            ]

        scheibenElemente =
            verwendeteScheiben
                |> List.indexedMap zeichneScheibe
                |> List.concat
    in
    if List.length verwendeteScheiben > 0 then
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
            ([ rechteck 180 40 900 "gray"
             , rechteck -30 100 20 "gray"
             ]
                ++ scheibenElemente
                ++ (if n.verschluss /= 0 then
                        [ rechteck (List.length verwendeteScheiben * 60 + 40) 60 60 "blue" ]

                    else
                        []
                   )
                ++ (if n.gewichtAnzeige then
                        [ anzeige 730 -45 n.gewichtTotal ]

                    else
                        []
                   )
            )

    else
        Svg.svg [] []


view : Model -> Html Msg
view model =
    let
        alleVerfügbarenScheiben =
            List.map (tdScheibe model) scheiben

        nModel =
            normiere model

        ( verwendeteScheiben, kilosÜbrig ) =
            berechneScheiben nModel.gewichtZuStecken nModel.verschluss nModel.scheiben
    in
    div []
        ([ form [ id "scheiben" ]
            [ table []
                [ tr []
                    (alleVerfügbarenScheiben
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
                [ ausgabeLinksText nModel |> text ]
            , div
                [ style "background-color" "orange"
                , style "display" "inline-block"
                ]
                [ ausgabeRechtsText nModel |> text ]
            ]
         , div
            [ style "width" "100%"
            , style "position" "relative"
            , style "user-select" "none"
            ]
            [ canvas
                [ style "background-color" "pink"

                -- , style "visibility" "hidden"
                , style "display" "block"
                , style "width" "100%"
                , style "height" "100%"
                , width 100
                , height 54
                ]
                []
            , zeichneStange nModel

            -- , Svg.Attributes.style "left" "0"
            -- , Svg.Attributes.style "position" "absolute"
            -- , Svg.Attributes.style "top" "0"
            ]
         , br [] []
         , text ("Verschlüsse: " ++ model.verschluss ++ " kg")
         , br [] []
         , text ("Stange: " ++ model.stange ++ " kg")
         , br [] []
         , text ("Gewichtanzeige: " ++ stringFromBool model.gewichtAnzeige)
         , br [] []
         , text ("Gewicht: \"" ++ model.gewichtTotalEingabe ++ "\" kg")
         , br [] []
         , text ("Ausgabe links: \"" ++ ausgabeLinksText nModel ++ "\"")
         , br [] []
         , text ("Verfügbare Scheiben: " ++ stringFromList (scheibenList model.scheiben))
         , br [] []
         , text ("Kilos übrig: " ++ deutschesFormat kilosÜbrig)
         , br [] []
         , text ("Verwendete Scheiben: " ++ stringFromList verwendeteScheiben)
         ]
            ++ List.concatMap (\( k, v ) -> [ br [] [], text (k ++ " kg Scheiben: " ++ String.fromInt v) ]) (Dict.toList model.scheiben)
        )


onSubmitCapture : Html.Attribute Msg
onSubmitCapture =
    let
        decoder =
            Json.Decode.map UpdateGewicht (Json.Decode.at [ "target", "0", "value" ] Json.Decode.string)
    in
    Html.Events.preventDefaultOn "submit" (Json.Decode.map (\msg -> ( msg, True )) decoder)
