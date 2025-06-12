module Uebung6 where

{-
Die Funktion guard aus Control.Monad ist leider nicht im Prelude, darf aber
in der Übung, im Midterm-Test und der Klausur vorausgesetzt werden,
sofern die konkrete Aufgabenstellung dies nicht ausschließt.
-}
import Control.Monad ( guard )

{-
Übung zur Vorlesung
Funktionale Programmierung
Sommersemester 2025
Übungsblatt 6
-}

{-
Ausgabe: 23.05.2025
Abgabe: keine
-}

{-
Aufgabe 6.1 - Listenmonade
-}

{-
a)
Implementieren Sie die Funktion
map :: (a -> b) -> [a] -> [b]
aus dem Prelude jeweils mittels Listenkomprehension,
Do- und Bind-Notation.
Nennen Sie die Funktionen mapLK, mapDo und mapBind.
-}

mapLK :: (a -> b) -> [a] -> [b]
mapLK f as = [f a | a <- as]

mapDo :: (a -> b) -> [a] -> [b]
mapDo f as = do
    a <- as
    return (f a)

mapBind :: (a -> b) -> [a] -> [b]
mapBind f as = as >>= \a -> return (f a)

{-
b)
Übersetzen Sie die Liste
pyTriples :: [(Int, Int, Int)]
in die Do- und Bind-Notation.
Nennen Sie die Listen pyTriplesDo und pyTriplesBind.
-}

pyTriples :: [(Int, Int, Int)]
pyTriples = [(a,b,c) |c <- [0..], b <- [0.. c], a <- [0.. b], (a < b) && (b < c), (a^2 + b^2) == c^2]

pyTriplesDo :: [(Int, Int, Int)]
pyTriplesDo = do
    c <- [0..]
    b <- [0..c]
    a <- [0..b]
    guard((a < b) && (b < c))
    guard((a^2 + b^2) == c^2)
    return (a, b, c)

pyTriplesBind :: [(Int, Int, Int)]
pyTriplesBind = 
    [0..] >>= \c ->
    [0..c] >>= \b ->
    [0..b] >>= \a ->
    guard((a < b) && (b < c)) >>
    guard((a^2 + b^2) == c^2) >>
    return (a, b, c)
 

{-
Aufgabe 6.2 - Monadeninstanzen
-}

{-
Gegeben sei der Datentyp Baum für nicht leere binäre Bäume, ohne innere Knoten.
-}

data Baum a = Blatt a | Knoten (Baum a) (Baum a) deriving (Show, Eq)

{-
a)
Machen Sie Baum zu einer Instanz der Typklassen Functor, Applicative und Monad.
-}



{-
b)
Implementieren Sie die Faltung
foldBaum :: (a -> b) -> (b -> b -> b) -> Baum a -> b
für den Datentyp Baum.

Anmerkung: Wenn in einer Aufgabenstellung gefordert wird die Faltung für einen
Datentyp zu implementieren, dann ist immer das Schema von Übungsblatt 5
gemeint, sofern explizit nichts anderes in der Aufgabenstellung steht.
-}

--foldBaum :: (a -> b) -> (b -> b -> b) -> Baum a -> b


{-
Aufgabe 6.3 - Maybe
-}

{-
Implementieren Sie eine Funktion
foo :: [Maybe a] -> Maybe [a],
die Nothing zurückgibt, wenn die Argumentliste ein Nothing enthält und sonst
alle Elemente der Liste aus dem Maybe "auspackt" und diese Liste wieder in ein
Maybe "einpackt".

Beispielaufrufe:
foo [Just 1, Just 4, Nothing, Just 3] ~> Nothing
foo [Just 1, Just 4, Just 3] ~> Just [1,4,3]
foo [] ~> Just []

Nutzen Sie zur Implementierung die do-Notation auf sinnvolle Weise.
-}

--foo :: [Maybe a] -> Maybe [a]


{-
Aufgabe 6.* - Kartesisches Produkt
-}

{-
Implementieren Sie eine Funktion
cartesianProduct :: [[a]] -> [[a]],
die das kartesische Produkt aller Elementlisten der Argumentliste berechnet.
Anstatt von n-Tupeln sollen aber Listen verwendet werden, um die Elemente des
n-stelligen kartesischen Produkts einfach abbilden zu können.

Beispielaufrufe:
cartesianProduct [] ~> [[]]
cartesianProduct [[1,2,3]] ~> [[1],[2],[3]]

cartesianProduct [[1,2,3], [4,5,6]] ~>
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

cartesianProduct [[1,2], [3,4], [5,6]] ~>
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]

cartesianProduct [[1,2], [3,4], [5,6], []] ~>
[]

cartesianProduct [[1,2], [3,4], [5,6], [7]] ~>
[[1,3,5,7],[1,3,6,7],[1,4,5,7],[1,4,6,7],[2,3,5,7],[2,3,6,7],[2,4,5,7],[2,4,6,7]]
-}

--cartesianProduct :: [[a]] -> [[a]]