module Uebung8 where

{-
Die Funktion guard aus Control.Monad ist leider nicht im Prelude, darf aber
in der Übung, im Midterm-Test und der Klausur vorausgesetzt werden,
sofern die konkrete Aufgabenstellung dies nicht ausschließt.
-}
import Control.Monad (guard)

{-
Aufgabe 8.1 - Rekursive Funktionen
-}

{-
a)
Definieren Sie eine Haskell-Funktion mit folgender Signatur:
splitYourAssCheeks :: [a] -> ([a],[a])
Die Funktion nimmt eine Liste als Argument und gibt ein Tupel zurück, bei dem die erste
Projektion die Liste aller Elemente mit einer geradem Position/einem geraden Index in
der Eingabeliste enthält und die zweite Projektion alle Elemente mit einer ungeraden
Position.
-}

{-
splitYourAssCheeks1 :: [a] -> ([a],[a])
splitYourAssCheeks (x : xs) = foldl (f x true) ([],[]) xs
	where
	f [] _ = (as, bs)
	f x true = (x : as, bs) 
	f x false = (as, x : bs)
-}

{-
splitYourAssCheeks2 :: [a] -> ([a],[a])
splitYourAssCheeks (x : xs) = foldl f ([],[], True) xs
	where
	f (as, bs, _) [] = (as, bs)
	f (as, bs, True) (x : xs) = if True then (x : as, bs)
	f (as, bs, False) (x : xs) = if False then (as, x : bs)
-}

splitYourAssCheeks3 :: [a] -> ([a], [a])
splitYourAssCheeks3 xs = f True xs
  where
    f _ [] = ([], [])
    f True (x:xs) = let (as, bs) = f False xs in (x:as, bs)
    f False (x:xs) = let (as, bs) = f True xs in (as, x:bs)

{-
b)
Definieren Sie eine Haskell-Funktion mit folgender Signatur:
tryZipWith :: (a -> b -> c) -> [a] -> [b] -> Either String [c]
Ähnlich zur Haskell-Funktion zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
soll tryZipWith für gleichlange, endliche Listen eine gegebene Funktion auf die Elemente
von zwei Listen entsprechend ihrer Indizes anwenden und das Ergebnis in der Either-
Monade kapseln. Haben die Listen unterschiedliche Länge, soll als Fehler in der Either-
Monade " Linke Liste zu kurz ." bzw. " Rechte Liste zu kurz ." zurückgegeben
werden.
Hinweis: Sie müssen unendliche Listen nicht betrachten.
-}

tryZipWith :: (a -> b -> c) -> [a] -> [b] -> Either String [c]
tryZipWith f [] [] = Right []
tryZipWith f as [] = Left "Rechte Liste zu kurz"
tryZipWith f [] bs = Left "Linke Liste zu kurz"
tryZipWith f (a:as) (b:bs) = Right (zipWith f as bs) 

{-
Aufgabe 8.2 - Datentypen, Faltungen und Typklassen
-}

{-
Gegeben sei der polymorphe Datentyp Exp für aussagenlogische Formeln über
beliebigen Variablenmengen.
Der Konstruktor VAR bettet Variablen in die Formeln ein.
Der Konstruktor VALUE bettet boolsche Werte in die Formeln ein.
Die anderen drei Konstruktoren entsprechen den Formelsymbolen ihres Bezeichners.
-}

data Exp a = VAR a | VALUE Bool | NOT (Exp a) | AND (Exp a) (Exp a) | OR (Exp a) (Exp a) deriving Show

-- True /\ ¬(x \/ y)
bspExp :: Exp Char 
bspExp = AND (VALUE True) (NOT (OR (VAR 'x') (VAR 'y')))

{-
a)
Implementieren Sie die Faltung
foldExp :: (a -> b) -> (Bool -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Exp a -> b 
für den Datentyp Exp.
-}

foldExp :: (a -> b) -> (Bool -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Exp a -> b 
foldExp var value not and or (VAR a) = var a
foldExp var value not and or (VALUE a) = value a
foldExp var value not and or (NOT a) = not (foldExp var value not and or a)
foldExp var value not and or (AND a b) = (foldExp var value not and or a) `and` (foldExp var value not and or b)
foldExp var value not and or (OR a b) = (foldExp var value not and or a) `or` (foldExp var value not and or b)


{-
b)
Definieren Sie eine Haskell-Funktion mit folgender Signatur:
eval :: (a -> Bool) -> Exp a -> Bool
Die Funktion nimmt eine Belegungsfunktion für die Variablen und eine aussagenlogische Formel
und evaluiert die Formel unter der gegebenen Belegung.
Das heißt, dass die Belegungsfunktion auf die Variablen angewendet wird, sodass die Formel keine 
Variablen mehr enthält und als boolscher Ausdruck ausgewertet werden kann.

Nutzen Sie zur Definition die Faltung foldExp auf sinnvolle Weise.

Beispielaufrufe:
eval (\v -> case v of 'x' -> True; 'y' -> False) bspExp
~>
False

eval (\v -> case v of 'x' -> False; 'y' -> False) bspExp 
~>
True
-}

eval :: (a -> Bool) -> Exp a -> Bool
eval f a = foldExp f id not (&&) (||) a

{-
c)
Definieren Sie eine Haskell-Funktion mit folgender Signatur:
elimOr :: Exp a -> Exp a 
Die Funktion soll alle Vorkommen des OR-Konstruktors eliminieren,
ohne den Wahrheitswert der Formel unter allen mögliche Belegungen zu ändern.
Hierzu soll die folgende Äquivalenz verwendet werden:
OR x y = NOT (AND (NOT x) (NOT y))

Nutzen Sie zur Definition die Faltung foldExp auf sinnvolle Weise.

Beispielaufruf:
elimOr bspExp 
~>
AND (VALUE True) (NOT (NOT (AND (NOT (VAR 'x')) (NOT (VAR 'y')))))
-}

elimOr :: Exp a -> Exp a 
elimOr a = foldExp var value not and f a
  where
    var a = VAR a
    value a = VALUE a
    not a = NOT a
    and a b = AND a b
    f a b = NOT (AND (NOT a) (NOT b))


{-
d)
Machen Sie Exp zu einer Instanz der Typklassen Functor, Applicative und Monad.

Tipp: foldExp kann Ihnen hier im Vergleich zum Pattern Matching sehr viel Schreibaufwand
ersparen.
-}

instance Functor Exp where
  fmap :: (a -> b) -> Exp a -> Exp b
  fmap f (VAR a) = VAR (f a)
  fmap f (VALUE b) = VALUE b
  fmap f (NOT e) = NOT (fmap f e)
  fmap f (AND e1 e2) = AND (fmap f e1) (fmap f e2)
  fmap f (OR e1 e2) = OR (fmap f e1) (fmap f e2)

instance Applicative Exp where
  pure :: a -> Exp a
  pure a = VAR a

  (<*>) :: Exp (a -> b) -> Exp a -> Exp b
  (<*>) (VAR f) a = fmap f a
  (<*>) (VALUE f) _ = VALUE f
  (<*>) (NOT f) a = NOT (f <*> a)
  (<*>) (AND fa fb) a = AND (fa <*> a) (fb <*> a)
  (<*>) (OR fa fb) a = OR (fa <*> a) (fb <*> a)

instance Monad Exp where
  (>>=) :: Exp a -> (a -> Exp b) -> Exp b
  (>>=) a f = foldExp (\var -> f var) VALUE NOT AND OR a

{-
Aufgabe 8.3 - Monaden
a)
Übersetzen Sie die gegebene Funktion 
foo :: Ord a => [a] -> [b] -> (a -> b -> Bool) -> [(a,b)]
von der do-Notation in die (>>=)-Notation, sowie in
die Listenkomprehensionsnotation.
Nennen Sie die übersetzen Funktionen fooB und fooL.
-}
foo :: Ord a => [a] -> [b] -> (a -> b -> Bool) -> [(a,b)]
foo as bs p = do
  a <- as
  b <- bs
  guard (p a b)
  a' <- as
  guard (a' < a)
  return (a', b)

fooB :: Ord a => [a] -> [b] -> (a -> b -> Bool) -> [(a,b)]
fooB as bs p = as >>= \a -> bs >>= \b -> guard(p a b) >>= \x -> as >>= \a' -> guard(a' < a) >>= \x' -> return (a', b)

fooL :: Ord a => [a] -> [b] -> (a -> b -> Bool) -> [(a,b)]
fooL as bs p = [(a, b) | a <- as, b <- bs, p a b, a' <- as, a' < a]


{-
Gegeben seien die aus der Vorlesung bekannten Haskell-Datentypen Nat und State, sowie die
Instanzen der Typklassen Functor, Applicative und Monad für State.
-}

data Nat = Z | S Nat deriving (Show, Eq)

newtype State s a = State {runS :: s -> (a, s)}

instance Functor (State s) where 
    fmap :: (a -> b) -> State s a -> State s b 
    fmap f (State g) = State $ \s1 -> 
        let (a, s2) = g s1 in (f a, s2)

instance Applicative (State s) where 
    pure :: a -> State s a 
    pure x = State $ \s -> (x, s)
    (<*>) :: State s (a -> b) -> State s a -> State s b 
    (State f) <*> (State h) = State $ \s1 -> 
        let (g, s2) = f s1 in let (a, s3) = h s2 in (g a, s3)

instance Monad (State s) where 
    (>>=) :: State s a -> (a -> State s b) -> State s b 
    (State f) >>= g = State $ \s1 -> 
        let (a, s2) = f s1 in let state = g a in runS state s2 

{-
Im Folgenden wollen wir die Zustandsmonade nutzen, um eine Stack-Maschine zu implementieren.
Hierzu seien ebenfalls die folgenden zustandsbehafteten Varianten der Funktionen push und
pop gegeben. Dabei entspricht der Zustand von State dem Stack.
-}

type Stack = [Int]

push :: Int -> State Stack ()
push i = State $ \ls -> ((), i : ls)

pop :: State Stack (Maybe Int)
pop = State $ \ls -> case ls of
  [] -> (Nothing, [])
  (x : xs) -> (Just x, xs)

{-
b)
Definieren Sie eine Haskell-Funktion mit der Signatur
clear :: State Stack Stack,
die den gesamten Stack zurückgibt und den leeren Stack als neuen Zustand setzt.
-}

clear :: State Stack Stack
clear = State(\s -> (s, []))

{-
c)
Definieren Sie eine Haskell-Funktion mit der Signatur
pushN :: [Int] -> State Stack (),
die eine Liste als Argument nimmt und alle Elemente derart auf den Stack legt, dass die
Reihenfolge aus der Liste erhalten bleibt und der Head der Liste als oberstes Element auf
dem Stack liegt. 

Nutzen Sie zur Definition die do-Notation auf sinnvolle Weise.
Der State-Konstruktor darf nicht verwendet werden.
-}

pushN :: [Int] -> State Stack ()
pushN [] = return () 
pushN (x : xs) = do 
  pushN xs 
  push x


{-
d)
Definieren Sie eine Haskell-Funktion mit der Signatur
popN :: Nat -> State Stack [Int],
die ein n vom Typ Nat als Argument nimmt und die obersten n Elemente vom Stack
entfernt und zurückgibt. Sollten weniger als n Elemente auf dem Stack liegen, so werden
alle Elemente des Stacks zurückgegeben. 

Nutzen Sie zur Definition die do-Notation auf sinnvolle Weise.
Der State-Konstruktor darf nicht verwendet werden.
-}

popN :: Nat -> State Stack [Int]
popN Z = return []
popN (S nat) = do
  x <- pop
  xs <- popN nat
  case x of 
    Nothing -> return []
    Just x -> return (x : xs)
