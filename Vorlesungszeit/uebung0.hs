module Main where

{-
Übung zur Vorlesung
Funktionale Programmierung
Sommersemester 2025
Übungsblatt 0
-}

{-
Ausgabe: 11.04.2025
Abgabe: Keine
-}

{-
Installieren Sie mittels GHCup die Haskell-Plattform (https://www.haskell.org/ghcup/)
auf ihrem Rechner.
Stellen Sie dabei sicher, dass ghc und ghci zu ihren Pfadvariablen
hinzugefügt sind.
-}

{-
Aufgabe 0.1 - Einführung GHCi
-}

{-
Wenn Sie dies lesen können, dann ist es Ihnen offensichtlich bereits gelungen
die Datei uebung0.hs in einem Texteditor Ihrer Wahl zu öffnen.

Glückwunsch, weiter so!

Öffnen Sie nun die Kommandozeile, navigieren Sie zum Ordner,
in dem diese Datei sich befindet, und laden Sie diese mit dem interaktiven
Modus des Glasgow Haskell Compiler (GHCi genannt), wie folgt:

ghci uebung0.hs

Sie sollten nun die folgende Ausgabe erhalten:

[1 of 1] Compiling Main             ( uebung0.hs, interpreted )
Ok, one module loaded.
*Main> 
-}

add :: Int -> Int -> Int
add x y = x + y

{-
Testen Sie nun die Funktion add, indem Sie z.B. "add 1 2" eingeben und mit ENTER bestätigen.
Das Ergebnis wird ausgegeben und Sie können weitere Funktionsaufrufe auswerten lassen.
-}

{-
Folgende Kommandos des GHCi haben sich als nützlich erwiesen:
• :load file (kurz :l) lädt die Datei file in den GHCi.
• :reload (kurz :r) lädt die aktuelle Datei neu ein.
  Nachdem Änderungen an dem Quelltext vorgenommen wurden,
  kann die aktuelle Datei mit :r leicht neu geladen werden.
• :type ausdruck (kurz :t) zeigt den Typ des Ausdrucks ausdruck an, z.B. :t add oder :t add 1 2.
• :kind typ (kurz :k) zeigt den Kind des Typs typ an, z. B. :k Int oder :k [].
• :info name (kurz :i) zeigt umfangreiche Informationen zu name an,
  z. B. :i True, :i Bool oder :i Eq.
• :help (kurz :h) öffnet die Hilfe mit weiteren nützlichen Befehlen.
• :quit (kurz :q) beendet den GHCi.
-}


{-
Aufgabe 0.2 - Fehlermeldungen des GHCi
-}

{-
Die folgende Aufgabe enthält eine Reihe von fehlerhaften Haskell-Ausdrücken.
Ziel dieser Aufgabe ist es, dass Sie sich mit den Fehlermeldungen des GHCi vertraut machen.
Laden Sie dazu diese Datei und interpretieren Sie die folgenden Ausdrücke mit dem GHCi.
Versuchen Sie die Fehlermeldungen nachzuvollziehen.
-}

--a) add 1 True => hier wird nach einem Int, nicht einem Bool gefragt
--b) add 4 3 2 => hier werden drei argumente übergeben, sind aber nur zwei definiert
--c) add 1 => hier wiederrum nur ein argument, sind aber zwei
--d) Add 1 2, Syntax beachtet auch groß und kleinschreibung
--e) foo 3 2 2, es wurde nie eine funktion foo definiert

{-
Aufgabe 0.3 - Einführung GHC
-}

main :: IO()
main = putStrLn "Hello World!"

{-
Mit dem Glasgow Haskell Compiler kann man auch ausführbare Dateien erzeugen.
Dazu muss eine Funktion main vom Typ IO () als Einstiegspunkt existieren.
Übersetzen Sie das Programm wie folgt:
ghc uebung0.hs
(Unter Arch basierten Distributionen: ghc -dynamic uebung0.hs)
Es entsteht eine ausführbare Datei mit gleichem Namen (uebung0) und der
Dateiendung .exe bzw. keiner Endung, je nach Betriebssystem.
Führen Sie diese Datei aus.
-}

{-
Aufgabe 0.4 - Hackage
-}

{-
Besuchen Sie die Seite https://hackage.haskell.org/.
Suchen Sie dort nach dem Paket base.
Finden Sie in dem Paket das Modul Prelude.
Hier finden Sie die Dokumentationen zu allen Funktionen, Datentypen,
etc. die Ihnen automatisch in Haskell zur Verfügung stehen.
Sie können weitere Module aus dem Paket base oder anderen Paketen mit der
Anweisung import nutzen. 
Manche Pakete müssen Sie vorher auf ihrem System installieren, hierzu stehen 
Tools wie Haskell Cabal (https://www.haskell.org/cabal/)
und der Haskell Tool Stack (https://docs.haskellstack.org/en/stable/) 
zur Verfügung. 
-}

{-
Aufgabe 0.5 - Hoogle
-}

{-
Besuchen Sie die Seite https://www.haskell.org/hoogle/.
Hier können Sie nach Funktionen, Datentypen und mehr suchen.
Finden Sie heraus, was der Operator $ macht.
-}

{-
Aufgabe 0.6 - Partielle Funktionsanwendung
-}

{-
a) Definieren Sie eine Funktion add4 :: Int -> Int, die eine ganze Zahl
vom Typ Int als Parameter erhält und die Summe aus 4 und dem Parameter
berechnet.
Nutzen Sie hierfür ausschließlich die Funktion successor und optional
den $ Operator, falls Sie sich ein paar Klammern sparen wollen.
-}

successor :: Int -> Int
successor = (+) 1

{-
Nutzen Sie hierzu den folgenden auskommentierten Code und vervollständigen Sie
die Definition. Testen Sie ihre Lösung anschließend, indem Sie sie in den GHCi
laden und ausführen.
-}

add4 :: Int -> Int
add4 x = successor(successor(successor(successor x)))
 
{-
b) Die Funktionsanwendung in Haskell ist per Definition bekanntermaßen
linksassoziativ.
Erweitern Sie die Definition der konstanten Funktion ten :: Int um die
impliziten Klammern.
-}

{-
(Hinweis: Die Explikation der Klammerung erhält natürlich die Semantik
des Ausdrucks.
Sollte die Datei uebung0.hs nicht mehr kompilieren, nachdem
sie die Klammern zur Definition von ten :: Int hinzugefügt haben, dann
sollten Sie ihre Lösung vielleicht noch einmal überdenken...)
-}

add4Ints :: Int -> Int -> Int -> Int -> Int
add4Ints a b c d = a + b + c + d

ten :: Int
ten = ((((add4Ints 1) 2) 3) 4)

{-
Aufgabe 0.* - konstante und nullstellige Funktionen
-}

{-
Definieren Sie eine Funktion funPlus, die isomorph zur Funktion add ist
und anstatt von zwei ganzen Zahlen, zwei einstellige Funktionen (z.B. funEins)
als Argumente nimmt.
-}

funEins :: () -> Int
funEins _ = 1

funZwei :: () -> Int
funZwei x = 2

funDrei :: () -> Int
funDrei () = 3

{-
Nutzen Sie hierzu den folgenden auskommentierten Code und vervollständigen Sie
die Definition. Testen Sie ihre Lösung anschließend, indem Sie sie in den GHCi
laden und ausführen.
-}

funPlus :: (() -> Int) -> (() -> Int) ->  Int
funPlus a b = a () + b ()