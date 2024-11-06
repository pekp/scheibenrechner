# Scheibenrechner

Dieses Repository enthält zwei Implementierungen eines Scheibenrechners.

- `scheibenrechner.html` – ursprüngliche Implementierung in JavaScript.
- `src/Main.elm` – Port nach Elm.

## Funktionalität

Von der Funktionalität her sind die Programme identisch. Das Elm-Programm
ist etwas komplizierter als es sein müsste, weil ich das "natürliche"
Formularverhalten von JavaScript in Elm nachgebaut habe – es ist nämlich
gar nicht so einfach, in Elm ein Formular zu generieren, dessen Inhalt erst
dann verarbeitet wird, wenn man es abschickt. In Elm löst standardmäßig jede
Tastatureingabe ein Event aus, das eine Verarbeitung triggert.

## Unterschiede

Es gibt ein paar kleine Unterschiede bei den Stylesheets, da war ich zu faul,
das anzupassen. Es sollte aber nicht allzu viel Arbeit sein.

## Entwicklungserfahrung

Im originalen JavaScript-Scheibenrechner steckt etwa ein Wochenende Arbeitszeit.
Es war mein erstes "ernsthaftes" JavaScript-Programm. Um mir JavaScript
anzueignen, las ich "JavaScript – The Good Parts" von Douglas Crockford und "Pro
JavaScript Techniques" von John Resig.

Im Elm-Port steckt etwa eine Woche Arbeit. Um mir Elm anzueignen, las ich
den offiziellen Elm Guide. Es ist mein erstes "ernsthaftes" Programm in
einer rein funktionalen Programmiersprache. Die Arbeit mit Elm hat mir viel
Freude bereitet, allerdings empfand ich sie auch deutlich mühsamer als die
Arbeit in reinem JavaScript. Trotzdem würde ich etwas größere Webanwendungen
wahrscheinlich eher in Elm entwickeln als in JavaScript, schon allein wegen der
sehr hilfreichen Fehlermeldungen und dem Typsystem.
