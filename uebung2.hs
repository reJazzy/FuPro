module Uebung2 where

{-
Übung zur Vorlesung
Funktionale Programmierung
Sommersemester 2025
Übungsblatt 2
-}

{-
Ausgabe: 25.04.2025
Abgabe: keine
-}


{-
Aufgabe 2.1 - Curryfizierung
-}

{-
a)
In Haskell können Funktionen selber Argument und Rückgabewert von Funktionen sein.
Man spricht hier von Funktionen höherer Ordnung.
Dies führt dazu, dass wir mehrstellige Funktionen meistens als einstellige Funktionen
betrachten, die eine Funktion als Rückgabewert liefert.
Alternativ könnte man diese Funktionen natürlich auch als Abbildung von n-Tupeln in den
Zieltyp auffassen.
Betrachten wir noch einmal das Beispiel aus der Vorlesung (Doberkat-Folie 14):
-}

b :: Num a => a -> a -> a
b x y = x + y

c :: Num a => (a, a) -> a
c (x, y) = x + y

{-
In dieser Aufgabe wollen wir uns einmal klar machen, dass die Funktionen b und c
isomorph sind. Hierzu implementieren Sie bitte die zwei Funktionen

cur :: ((a, b) -> c) -> a -> b -> c

und

uncur :: (a -> b -> c) -> (a, b) -> c.

Vergewissern Sie sich durch sinnvolle Tests, dass gilt:

cur c = b
uncur b = c

bzw.

cur (uncur b) = b
uncur (cur c) = c

Können Sie Vorteile der "curryfizierten" Funktion b gegenüber der isomorphen Definition
der Funktion c nennen?
-}

--cur :: ((a, b) -> c) -> a -> b -> c


--uncur :: (a -> b -> c) -> (a, b) -> c


{-
b)
Implementieren Sie eine Funktion
foo :: (a -> b, a -> a) -> a -> b,
die ein Paar von Funktionen, sowie einen "Startwert" als Argumente nimmt und
die Funktion aus der zweiten Projektion zweimal auf den Startwert anwendet und
auf das Ergebnis die Funktion aus der ersten Projektion.

Implementieren Sie dann eine Funktion
bar :: Int -> (a -> b, a -> a) -> a -> b,
die zusätzlich über die Anzahl der Applikationen der zweiten Projektion abstrahiert.

Beispielaufrufe:
foo (head, tail) [1..] ~> 3
bar 2 (head, tail) [1..] ~> 3

Tipp: Die Funktion
applyNtimes :: (a -> a) -> Int -> a -> a
von Blatt 1 kann hier hilfreich sein.
-}

--foo :: (a -> b, a -> a) -> a -> b


--bar :: Int -> (a -> b, a -> a) -> a -> b


{-
Aufgabe 2.2 - Typklassen
-}

{-
Implementieren Sie eine Funktion h, die den Typ
Read a => (b -> c) -> (a -> b) -> String -> c
hat.
Die Typklasse Read wurde in der Vorlesung eingeführt (Doberkat-Folien 45 & 46).

Tipp: Für eine geeignete Implementierung könnten Beispielaufrufe zum Testen
wie folgt aussehen:
• h show (+1.5) "2.5" ~> "4.0"
• h (+1) (+1) "2" ~> 4
-}

--h :: Read a => (b -> c) -> (a -> b) -> String -> c


{-
Aufgabe 2.3 - Listen
-}

{-
a)
Ein Tripel (a, b, c) ∈ N^3 ist ein pythagoreisches Tripel genau dann,
wenn a < b < c und a^2 + b^2 = c^2 gelten.
Zum Beispiel handelt es sich bei (3, 4, 5) um ein pythagoreisches Tripel.

Implementieren Sie eine Haskell-Funktion pyTriples :: [(Int,Int,Int)],
die die unendliche Liste aller pythagoreischen Tripel erzeugt.

Testen Sie Ihre Lösung mittels Beispielaufrufen, wie z.B.:
take 5 pyTriples ~> [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17)]

Sollte ein solcher Beispielaufruf nicht terminieren (innerhalb weniger Sekunden...),
dann sollten Sie Ihre Lösung überdenken!
-}

--pyTriples :: [(Int, Int, Int)]
--pyTriples = 


{-
b)
Implementieren Sie eine Haskell-Funktion collapse :: (a -> a -> a) -> a -> [a] -> a,
die einen kommutativen Operator op :: a -> a -> a, einen Startwert e :: a
und eine Eingabeliste xs :: [a] erhält. Für die leere Liste soll der Startwert
zurückgegeben werden, während bei einer nichtleeren Liste der Operator auf das erste
Listenelement und den Wert von collapse für die Restliste angewandt wird.

Sie dürfen außer dem Operator ($) (um evtl. Klammern zu sparen) keine (!) anderen Funktionen
zur Definition nutzen.

Beispielsweise sollte der Ausdruck collapse (+) 0 [1,2,3,4] zu dem Wert 10 reduzieren.
-}

--collapse :: (a -> a -> a) -> a -> [a] -> a



{-
Aufgabe 2.* - Entfaltungen
-}

{-
Betrachten Sie erneut die Funktionen foo und bar aus Aufgabe 2.1.
Anstatt einen einzelnen Wert durch mehrmalige Anwendung der Projektionen zu
berechnen, sollen Sie dieses Mal alle Werte berechnen.
Implementieren Sie hierzu eine Funktion
buildList :: (a -> b, a -> a) -> a -> [b]
die ähnlich funktioniert wie bar, aber eine unendliche Liste zurückgibt,
in der das Element mit Index i der i-fachen Applikation der zweiten Projektion vor der
Applikation der ersten Projektion entspricht.

Beispielaufrufe:
take 10 $ buildList (show, (+1)) 0
~>
["0","1","2","3","4","5","6","7","8","9"]

take 10 $ buildList (id, (+2)) 0
~>
[0,2,4,6,8,10,12,14,16,18]
-}

--buildList :: (a -> b, a -> a) -> a -> [b]
