module Uebung1 where

{-
Übung zur Vorlesung
Funktionale Programmierung
Sommersemester 2025
Übungsblatt 1
-}

{-
Block: 1
Ausgabe: 17.04.2025
Abgabe: keine
-}

{-
Aufgabe 1.1  - Rekursive Funktionen
-}

{-
a) Gegeben sei die Fakultätsfunktion fakt1 :: Int -> Int von den Vorlesungsfolien.
Laden Sie uebung1.hs im GHCi und finden Sie das größte Argument vom Typ Int,
für das die Funktion fakt1 ein sinnvolles Ergebnis liefert.

=> Hier ist es fakt1(20)

Ändern Sie den Typ von Int -> Int zu Integer -> Integer und probieren Sie ein paar
größere Eingaben aus.

=> Hier rechnet fakt1(x <= 50) noch immer richtig

Vergleichen Sie die Informationen, die Ihnen der GHCi mittels des Kommandos :i
zu den Typen Int und Integer liefert.

=> Während Int einen festen Wertebereich von 32 oder 64Bit hat, wächst Integer dynamisch im Speicher mit und hat damit keine feste Obergrenze im Speicher

-}

fakt1 :: Int -> Int
fakt1 n =
  if n == 0
    then 1
    else n * fakt1 (n - 1)

{-

b) Die Türme von Hanoi (https://de.wikipedia.org/wiki/Türme_von_Hanoi) sind ein
bei Informatikern beliebtes Puzzle, um rekursive Algorithmen zu motivieren.
Man kann die Anzahl an Schritten, die benötigt wird um n Scheiben von einem Stab
zu einem anderen zu bewegen, nach folgendem Rekursionsprinzip berechnen:

           { 1                  , wenn n = 1
hanoi(n) = {
           { 2 * hanoi(n-1) + 1 , sonst

Implementieren Sie eine Haskell Funktion hanoi :: Int -> Int, die für alle
Eingaben n > 0 die Anzahl an Schritten nach dem obigen Rekursionsprinzip berechnet,
um n Scheiben von einem Stab zum anderen zu bewegen.
Für alle Eingaben n <= 0 soll 0 zurückgegeben werden.

=> Implementierung ist einfach, wenn man sich die a) anschaut

-}

hanoi :: Int -> Int
hanoi n =
  if n == 1
    then 1
    else 2 * hanoi (n - 1) + 1

{-

c) Implementieren Sie eine Haskell Funktion
applyNtimes :: (a -> a) -> Int -> a -> a

\* die als erstes Argument eine einstellige Funktion f nimmt, => Erster Parameter: Funktion
\* als zweites Argument eine positive ganze Zahl n und => Zweiter Parameter: Rekursionsaufrufe der Funktion
\* als drittes Argument ein Element x auf das die Funktion aus dem ersten Argument
angewendet werden kann. => Dritter Parameter: Eingabe Wert der Funktion

Die Funktion applyNtimes soll dann die Funktion f aus dem ersten Argument n mal auf
das Element aus dem dritten Argument anwenden.

Die Funktion soll also z.B.  wie folgt ausgewertet werden:
applyNtimes f 0 x ~> x
applyNtimes f 2 x ~> f (f x)
applyNtimes f 5 x ~> f (f (f (f (f x))))
usw. ...
-}

applyNtimes :: (a -> a) -> Int -> a -> a
applyNtimes f n x =
  if n == 0
    then x
    else applyNtimes f (n - 1) (f x)

{-
Aufgabe 1.2 - Lambda Ausdrücke

a) Geben Sie einen Lambda Ausdruck für die Funktion
flip :: (a -> b -> c) -> b -> a -> c
aus dem Prelude an.

Die Funktion flip nimmt eine Funktion f :: a -> b -> c und gibt eine Funktion
f' :: b -> a -> c zurück.

=> flip(f(a,b)) = f(b,a)
=> f(a,b) = c
=> f(b,a) = c'

Bedenken Sie, dass der Funktionspfeil rechtsassoziativ ist.
Nennen Sie den Lambda Ausdruck flip'.
-}

flip' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip' = \f b a -> f a b


{-

b) Geben Sie einen Lambda Ausdruck für die Funktion
curry :: ((a, b) -> c) -> a -> b -> c
aus dem Prelude an.

In den Doberkat-Folien wurde bereits auf Folie 14 die sogenannte Curryfizierung
angesprochen. Die Funktion curry implementiert die Curryfizierung (Kaskadierung) einer Funktion.

Sie nimmt eine Funktion f :: (a, b) -> c als Argument und gibt eine Funktion
f' :: a -> b -> c zurück.

=> ???

Bedenken Sie, dass der Funktionspfeil rechtsassoziativ ist.
Nennen Sie den Lambda Ausdruck curry'.

-}

curry' :: ((a, b) -> t) -> a -> b -> t
curry' = \f a b -> f (a, b)

{-

c) Geben Sie einen Lambda Ausdruck für eine Funktion an, der zwei Argumente b und e
nimmt und durch sinnvollen Aufruf von applyNtimes b^e berechnet.

Nennen sie den Lambda Ausdruck exponential:
Also z.B.
exponential 2 0 ~> 1
exponential 2 4 ~> 16
exponential 3 2 ~> 9
usw. ...

-}

exponential :: Integer -> Int -> Integer
exponential = \b e -> applyNtimes(*b) e 1

{-
=> applyNtimes(*b) e 1, hier heisst (*b), dass b *-mal genommen wird mit dem linken argument, auf das es wartet
=> heisst bei uns, es wird e-mal b * b * b * ... * b gerechnet

=> Dann passiert Folgendes:

=> Du wendest * nur auf das rechte Argument b an

=> Das ergibt eine neue Funktion, die auf das linke Argument wartet


-}