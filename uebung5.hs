module Uebung5 where

{-
Übung zur Vorlesung
Funktionale Programmierung
Sommersemester 2025
Übungsblatt 5
-}

{-
Ausgabe: 16.05.2025
Abgabe: keine
-}

{-
!!!!!!!!!!!
Dieses Blatt führt einen neuen Datentyp Nat für natürliche Zahlen ein.
Viele der Aufgaben können natürlich gelöst werden, indem man z.B. Funktionen
nat2Int und int2Nat implementiert.
Das ist aber nicht Sinn der Aufgaben und würde in der Klausur explizit
ausgeschlossen werden (keine Punkte geben).
Sie sollen hier lernen/üben Faltungen für beliebige algebraische Datentypen
zu implementieren und anzuwenden. Nutzen Sie also die Gelegenheit.
Falls Sie die Faltungen tatsächlich nicht implementiert bekommen, können Sie
die Aufgaben trotzdem per Pattern Matching lösen. Dann raten wir Ihnen aber
dringend die Übungen wahrzunehmen und Fragen zu stellen.
!!!!!!!!!!!
-}

{-
Aufgabe 5.1 - Nat
-}

data Nat = Z | S Nat deriving (Show, Eq)

{-
Gegeben sei der Haskell-Datentyp Nat für natürliche Zahlen.
Die natürlichen Zahlen 0,1,2,3 entsprechen dann z.B. den folgenden Nat-Termen:
-}

nullN :: Nat
nullN = Z

einsN :: Nat
einsN = S Z

zweiN :: Nat
zweiN = S $ S Z

dreiN :: Nat
dreiN = S $ S $ S Z

{-
a)
Implementieren Sie eine Haskell-Funktion
foldNat :: b -> (b -> b) -> Nat -> b,
die als erstes Argument eine Interpretation des Z-Konstruktors und
als zweites Argument eine Interpretation des S-Konstruktors nimmt und
eine Funktion zurückgibt, die einen Nat-Term unter dieser Interpretation
auswertet.
Die Faltung foldNat soll also alle Vorkommen des Z-Konstruktors durch das erste
Argument und alle Vorkommen des S-Konstruktors durch das zweite Argument
ersetzen.

Beispielauswertungen:

foldNat f g (Z) ~> f
foldNat f g (S $ Z) ~> g f
foldNat f g (S $ S Z) ~> g $ g f
...

-}

foldNat :: b -> (b -> b) -> Nat -> b
foldNat f g Z = f
foldNat f g (S x) = g (foldNat f g x)

{-
b)
Implementieren Sie eine Haskell-Funktion
add :: Nat -> Nat -> Nat,
die zwei natürliche Zahlen addiert.
Nutzen Sie hierfür die Faltung foldNat auf sinnvolle und nicht triviale Weise.
-}

add :: Nat -> Nat -> Nat
add x y = foldNat x S y  

{-
c)
Implementieren Sie eine Haskell-Funktion
nat2binär :: Nat -> [Bool],
die eine natürliche Zahl in die Binärzahldarstellung aus Übung 3
(Listen von Bools, LSB 0) übersetzt.
Nutzen Sie hierfür die Faltung foldNat auf sinnvolle und nicht triviale Weise.
-}

nat2binär :: Nat -> [Bool]
nat2binär nat = foldNat [False] incrBin nat
  where 
    incrBin :: [Bool] -> [Bool]
    incrBin [] = [True]  
    incrBin (False:xs) = True : xs
    incrBin (True:xs)  = False : incrBin xs



{-
Aufgabe 5.2 - Maybe & Either
-}

{-
a)
Implementieren Sie eine Haskell-Funktion
predNat :: Nat -> Maybe Nat,
die den Vorgägner einer natürlichen Zahl berechnet, sofern dieser existiert.
Wenn kein Vorgänger existiert soll Nothing zurückgegeben werden.

Anmerkung: predNat darf hier gerne mittels Pattern Matching definiert werden.
           Außer Sie wollen die *-Aufgabe vorziehen.
-}

--predNat :: Nat -> Maybe Nat



{-
b)
Implementieren Sie eine Haskell-Funktion
minus :: Nat -> Nat -> Maybe Nat,
die zwei natürliche Zahlen subtrahiert. Sollte das zweite Argument größer
als das erste sein, so soll Nothing zurückgegeben werden.
Nutzen Sie hierfür die Faltung foldNat auf sinnvolle und nicht triviale Weise.

Tipp:
Guards oder ähnliches sind nicht notwendig, der "Fehlerfall" kann durch
foldNat korrekt gehandhabt werden.
-}

--minus :: Nat -> Nat -> Maybe Nat


{-
c)
Implementieren Sie eine Haskell-Funktion
elemAtIndex :: Int -> [a] -> Either String a,
die als erstes Argument einen Index und als zweites Argument eine Liste nimmt
und das Element der Liste an dem Index zurückgibt.

Hierbei können Fehler auftreten, die durch die Verwendung von (Either String)
abgefangen und durch sinnvolle Fehlermeldungen beschrieben werden sollen.
Sollte der Index größer als die Länge der Liste sein, so soll der String
"Fehler: Index zu groß" im Either zurückgegeben werden.
Sollte der Index negativ sein, so soll der String
"Fehler: Negativer Index" im Either zurückgegeben werden.
-}

--elemAtIndex :: Int -> [a] -> Either String a


{-
Aufgabe 5.3 - Binäre Bäume
-}

{-
In der Vorlesung wurde auf den Doberkat-Folien S.224ff ein Datentyp für
binäre Bäume vorgestellt und genutzt.
Wir behandeln hier eine Abwandlung der Datentyps von den Folien für 
nicht leere binäre Bäume und nennen ihn BinBaum.

Des weiteren sei der Datentyp Baum für Bäume beliebigen Ausgrads gegeben.
-}

data BinBaum a = BinBlatt a | BinKnoten a (BinBaum a) (BinBaum a) deriving (Show, Eq)

data Baum a = Knoten a [Baum a] deriving (Show, Eq)

beispielBinBaum :: BinBaum Int
beispielBinBaum = BinKnoten 4
  (BinKnoten 2 (BinBlatt 1) (BinBlatt 3))
  (BinKnoten 6 (BinBlatt 5) (BinKnoten 7 (BinBlatt 8) (BinBlatt 9)))

beispielBaum :: Baum Int
beispielBaum = Knoten 4 [Knoten 2 [Knoten 1 [], Knoten 3 []], Knoten 6 [Knoten 5 [], Knoten 7 [Knoten 8 [], Knoten 9 []]]]

{-
a)
Implementieren Sie eine Haskell-Funktion
convert :: BinBaum a -> Baum a,
die einen binären Baum in einen Baum beliebigen Ausgrads übersetzt.

Beispielaufruf:
convert beispielBinBaum ~> beispielBaum (siehe Definitionen oben)
-}

--convert :: BinBaum a -> Baum a


{-
b)
Implementieren Sie eine Haskell-Funktion
preFold :: (a -> b) -> (a -> b -> b -> b) -> BinBaum a -> b,
die als erstes Argument eine Interpretation des BinBlatt-Konstruktors und als
zweites Argumgent eine Interpretation des BinKnoten-Konstruktors nimmt und
eine Funktion zurückgibt, die einen BinBaum-Term unter dieser Interpretation
auswertet.

Beispielauswertung:
preFold f g (BinBlatt x) ~> f x
preFold f g (BinKnoten x (BinBlatt l) (BinBlatt r)) ~> g x (f l) (f r)
...
-}

--preFold :: (a -> b) -> (a -> b -> b -> b) -> BinBaum a -> b



{-
c)
Implementieren Sie eine Haskell-Funktion
preorder :: BinBaum a -> [a],
die einen binären Baum in preorder-Reihenfolge druchläuft und die Knotenelemente
in eine Liste schreibt.
Nutzen Sie hierfür die Faltung preFold auf sinnvolle und nicht triviale Weise.

Beispielaufruf:
preorder beispielBinBaum ~>
[4,2,1,3,6,5,7,8,9]
-}

--preorder :: BinBaum a -> [a]


{-
d)
Implementieren Sie eine Haskell-Funktion
knotenSumme :: Num a => BinBaum a -> a,
die die Summe aller Knoten eines binären Baums berechnet.
Nutzen Sie hierfür die Faltung preFold auf sinnvolle und nicht triviale Weise.

Beispielaufruf:
knotenSumme beispielBinBaum ~>
45
-}

--knotenSumme :: Num a => BinBaum a -> a




{-
Aufgabe 5.* - Destruktor
-}

{-
Implementieren Sie die Funktion
predNat :: Nat -> Maybe Nat
aus Aufgabe 5.2 a) erneut, nutzen Sie aber dieses Mal
foldNat auf sinnvolle und nicht triviale Weise und nennen Sie die Funktion
predNat'.
-}

--predNat' :: Nat -> Maybe Nat

