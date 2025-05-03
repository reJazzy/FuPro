module Tutorial1 where

{-
Tutorium 1
23.04.2025
-}

{-
##### Rekursion, Funktionen, Lambda #####
-}

{-
Fibonacci-Zahlen sind mathematisch wie folgt definiert:

               { 0                               , wenn n=0
fibonacci(n) = { 1                               , wenn n=1
               { fibonacci(n-1) + fibonacci(n-2) , sonst

Wir können das auf verschiedene Weisen in Haskell umsetzen
-}

-- Pattern Matching
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- If-then-else
fibonacci' :: Int -> Int
fibonacci' n = if n == 0
    then 0
    else if n == 1
        then 1
        else fibonacci' (n-1) + fibonacci' (n-2)

-- Guards
fibonacci'' :: Int -> Int
fibonacci'' n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibonacci'' (n-1) + fibonacci'' (n-2)

{-
- Wichtig: Guards und if-then-else überprüfen *Bedingungen*
- Also Boolean-Werte
- Pattern Matching dagegen versucht die Argumente auf das gegebene Muster zu matchen
- fibonacci (n==0) o.ä. würde daher nicht gehen, da das eine Bedingung ist, kein Muster
-}

{-
Anderes Beispiel, die Collatz-Funktion:

             { n/2     , wenn n gerade
collatz(n) = {
             { 3*n + 1 , wenn n ungerade

(Am Rande für Neugierige, ein interessantes Video zur Collatz-Vermutung:
https://youtu.be/094y1Z2wpJg)
-}

collatz :: Int -> Int
collatz n
    | even n = n `div` 2
    | otherwise = 3*n + 1

{-
Mit applyNtimes können wir Elemente von beliebige Collatz-Folgen berechnen:
-}

applyNtimes :: (a -> a) -> Int -> a -> a
applyNtimes f 0 x = x
applyNtimes f n x = f $ applyNtimes f (n-1) x

-- Berechnet die n-te Zahl aus der Collatz-Folge mit Startzahl 7
f :: Int -> Int
f n = applyNtimes collatz n 7

{-
- Mit Lambda-Ausdrücken können wir Funktionen "anonym" angeben
- Mathematische Syntax: λx.y
- In Haskell: \x -> y
-}

test = (\n -> applyNtimes collatz n 7) 4

-- Macht das gleiche wie f
f' :: Int -> Int
f' = \n -> applyNtimes collatz n 7

{-
- Mit Lambda-Ausdrücken und applyNtimes können wir auch die Fakultätsfunktion darstellen
- Wir sehen: Funktionen sind in Haskell auch nur normale Werte, sie können übergeben werden
  und bearbeitet werden wie alle anderen Werte auch
-}

fac n = applyNtimes (\f -> \n -> n * f (n + 1)) n (\_ -> 1) 1


{-
##### Funktionen mit mehreren Argumenten #####
-}

{-
- Funktionen mit mehreren Argumenten stellen wir in Haskell als
  "verschachtelte" Funktionen dar
- Addition kann man sich also vorstellen als eine Funktion, die eine Zahl nimmt und eine Funktion
  zurückgibt, die diese Zahl addiert
- Z.B.: add 3 :: Int -> Int ist die Funktion, die 3 addiert
- (add 3) 2 :: Int ist dann (addiere 3) von 2, also 5
-}

add :: Int -> (Int -> Int)
add x y = x + y

{-
- Wir können stattdessen Funktionen mit mehreren Argumenten auch mit Tupeln darstellen
- In Haskell nutzen wir aber meistens die erste Variante
- Mehr dazu kommt noch unter dem Begriff "Curryfizierung"
-}

add' :: (Int, Int) -> Int
add' (x, y) = x + y

{-
Die Ackermannfunktion ist eine extrem schnell wachsende mathematische Funktion.
Wir betrachten folgendes Rekursionsprinzip für die Ackermannfunktion:

             { m + 1                 , wenn n = 0
             {
ack(n, m) =  { ack(n-1, 1)           , wenn m = 0
             {
             { ack(n-1, ack(n, m-1)) , sonst
-}

ack :: Integer -> Integer -> Integer
ack 0 m = m + 1
ack n 0 = ack (n-1) 1
ack n m = ack (n-1) $ ack n (m-1)
