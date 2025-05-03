module Tutorial2 where

{-
##### Listen #####
-}

{-
Eine informelle Beschreibung einer Liste von Zahlen könnte sein:

1. "Ende" ist eine Liste
2. Wenn xs eine Liste und x eine Zahl ist: dann ist x, xs auch eine Liste

- Das ist eine induktive Definition!
- Beispiele:
    - Ende
    - 3, (2, (5, Ende)) = 3, 2, 5, Ende
    - 1, 2, 3, 4, Ende
-}

{-
In Haskell sind Listen so definiert (im GHCI :i [] eingeben):

data [a] = [] | a:[a]

=> polymorph, aber dieselbe Idee

Wegen diesem Aufbau kann Pattern Matching Listen auch immer nur "von vorne her aufbrechen":

foo :: [Int] -> [Int]
foo [] = []
foo (x:xs) = (x*42) : foo xs

x ist hier dann das erste Element der Liste.
-}

{-
Die Funktion len berechnet die Länge einer Liste und ist rekursiv wie folgt definiert:
Die leere Liste ([] - Fall) hat die Länge 0.
Jede nicht leere Liste ((:) - Fall) ist um 1 länger als die Liste ohne ihr erstes Element.

Wenn wir den (:)-Fall als (head : tail) lesen, lässt sich der zweite Fall etwas einfacher formulieren:
Jede nicht leere Liste ((:) - Fall) ist um 1 länger als ihr tail.

Mittels Pattern Matching können wir die Fallunterscheidung einfach in Haskell umsetzen.
-}

len :: [a] -> Int
len [] = 0
--len (hd : tl) = 1 + len tl
{- Da hd auf der rechten Seite nicht verwendet wird, können wir die Wildcard _ nutzen: -}
len (_ : tl) = 1 + len tl

{-
Die Schreibweise [1,2,3] ist für uns idR einfacher zu lesen als 1 : 2 : 3 [].
Daher kommt Haskell mit einigen Konventionen bzgl. Listen daher, die gerne als 
"syntaktischer Zucker" bezeichnet werden.
Wir können also z.B. für Listen 1 : 2 : 3 [] in Haskell auch [1,2,3] schreiben.
Intern arbeitet der GHC aber weiterhin auf der Darstellung 1 : 2 : 3 [].

Auch die Listenkomprehension
[(a,b) | a <- [1..100], b <- [1..100], a < b]
ist syntaktischer Zucker. Wir werden das genauer besprechen, wenn wir zu Monaden kommen.

Ebenfalls nützlicher syntaktischer Zucker ist die Schreibweise
[0..10] 
für die Liste
[0,1,2,3,4,5,6,7,8,9,10].
Wir können auch unendliche Listen auf diese Art definieren:
[0..]
-}

{-
Da Listen induktiv (bzw. rekursiv) definiert sind, kann uns die Struktur der Listen bei der
Definition von (rekursiven) Funktionen auf Listen helfen.
Um uns das näher anzuschauen, werden wir ein paar Funktionen aus dem Prelude selber implementieren:
-}

{-
Die Funktion map' (wir müssen einen anderen Namen als map nehmen, da dieser bereits im Prelude definiert ist),
nimmt eine Funktion f :: a -> b und eine Liste [a] als Argumente und wendet f auf jedes Element der Liste an,
um eine Liste [b] zurückzugeben.
-}
map' :: (a -> b) -> [a] -> [b]
--Wir bilden die leere Liste ([] :: [a]) auf die leere Liste ([] :: [b]) ab.
--Der Fall der leeren Liste entspricht also dem Rekursionsabbruch und wir müssen irgendeinen Wert vom Typ [b] zurückgeben.
map' f [] = []
{-
Im Fall der nicht leeren Liste (hd : tl) müssen wir f auf den head und alle weiteren Elemente im tail anwenden. 
Die Struktur der Liste gibt uns hier also einen Hinweis, wo der rekursive Aufruf hin muss.

map' f (x : xs) = ?

Da für (f :: a -> b) (map f :: [a] -> [b]) gilt, wissen wir, dass wir auf der rechten Seite eine Liste konstruieren müssen 
und diese nicht leer ist:

map' f (x : xs) = ? : ?

Links vom (:) müssen wir also ein Element vom Typ b konstruieren und rechts vom (:) ein Element vom Typ [b].
Im Kontext haben wir (f :: a -> b), (x :: a) und (xs :: [a]). Offensichtlich müssen wir f auf x anwenden, um ein
Element vom Typ b zu konstruieren:

map' f (x : xs) = f x : ?

Aber wie bekommen wir nun etwas vom Typ [b]? 
Naja, wir werden wohl map' rekursiv auf f und xs aufrufen müssen.
-}
map' f (x : xs) = f x : map' f xs

{-
Die Funktion kann man auch mittels Listenkomprehension definieren, allerdings "verschleiert" uns der syntaktische Zucker
so die Rekursion und hilft uns nicht zu verstehen, wie die Struktur von Listen uns das Rekursionsprinzip vorgeben kann.
-}
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [f x | x <- xs]

{-
Betrachten wir als nächstes die Funktion zip :: [a] -> [b] -> [(a,b)] aus dem Prelude,
die zwei Listen nimmt und ihre Elemente positionsweise als Paare in eine neue Liste schreibt.
Wir nennen die Funktion im Folgenden zip'.
Wenn die beiden Argumentlisten unterschiedlich lang sind, so ist die Rückgabe von zip' so lang wie die kürzeste Liste.

Wir stellen also zwei Dinge fest:
-Ähnlich wie map' bildet zip' Listen auf Listen ab.
-Allerdings ist zip' rekursiv auf zwei Listen definiert.

Wir müssen also auf zwei Listen Pattern Matchen. Da es zwei Fälle im Pattern Match auf Listen gibt,
muss zip' 2 * 2 = 4 Fälle betrachten:
-}

zip' :: [a] -> [b] -> [(a,b)]
--Die Rückgabe ist so lang, wie die kürzeste Liste. Also wenn eins der Argumente leer ist, ist die Rückgabe leer.
zip' [] [] = []
zip' [] (x : xs) = []
zip' (x : xs) [] = []
{-
Die Frage ist nun, wie wir mit zwei nicht leeren Listen umgehen:

zip' (x:xs) (y:ys) = ?

Wie bereits erwähnt, bildet zip' ähnlich wie map' Listen auf Listen ab.
Wir wissen also bereits, dass auf der rechten Seite eine Liste vom Typ [(a,b)] konstruiert werden muss.

zip' (x:xs) (y:ys) = ? : ?

Wenn wir das Muster aus map' erneut anwenden wollen, dann müssen links vom (:) die Elemente x und y behandelt werden
und rechts vom (:) der rekursive Aufruf stehen.
Und dann sind wir auch schon fertig:
-}
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys


{-
Da die ersten drei Fälle von zip' redundant sind, kann man die Definition durch "umsortieren" deutlich kürzen:
-}
zip'' :: [a] -> [b] -> [(a,b)]
zip'' (x:xs) (y:ys) = (x, y) : zip' xs ys
zip'' _ _ = []

{-
Wenn wir die Definition von zip' analog zu map' als Listenkomprehension darstellen, haben wir allerdings eine andere Funktion
definiert:
-}
listProd :: [a] -> [b] -> [(a,b)]
listProd xs ys = [(a,b) | a <- xs, b <- ys] 
{-
listProd ist das kartesische Produkt von zwei Listen und tut damit was ganz anderes, als die Elemente positionsweise als Paare
zusammenzufügen.
Es bietet sich auch nicht an zip mittels Listenkomprehension zu implementieren.
-}

{-
Die Funktion zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] aus dem Prelude ist eine Verallgemeinerung von zip.
Anstatt des Tupel-Konstruktors (,) :: a -> b -> (a, b), wird hier eine beliebige
Funktion f :: a -> b -> c zum positionsweise Zusammenfügen der Elemente verwendet.

Aber Moment: Das heißt, dass aus Sicht der Rekursion zipWith komplett analog zu zip 
definiert werden kann! Wir müssen lediglich über ein weiteres Argument vom Typ (f :: a -> b -> c)
abstrahieren und jedes Vorkommen von (,) durch f ersetzen:
-}
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' f _ _ = []

{-
len, map, zip und zipWith haben alle gemeinsam, dass ihre (rekursive) Definition der Struktur ihrer Argumentliste(n) folgt.
map, zip und zipWith bilden dabei Listen auf Listen ab, wodurch (:) uns indirekt sagt, wo der rekursive Aufruf hin muss.
len bildet Listen auf Int ab, sodass wir hier keinen Konstruktor haben, der uns sagt, wo der rekursive Aufruf hin muss.
Die Struktur ist aber trotzdem dieselbe:
Die leere Liste wird auf einen konstanten Wert des Zielbereichs abgebildet (Rekursionsabbruch) und im Fall der nicht leeren Listen
müssen wir die Rückgabe des rekursiven Aufrufs mit einem Element der Liste "verarbeiten" (Rekursionsschritt).

Können wir dieses Rekursionsprinzip irgendwie verallgemeinern? 
Brauchen wir gar nicht, denn wir kennen das bereits!
Das ist genau, was 
foldr :: (a -> b -> b) -> b -> [a] -> b
tut!
Das Argument vom Typ (cons :: a -> b -> b) sagt uns, wie wir ein Element der Liste und die Rückgabe des rekursiven Aufrufs handhaben.
Das Argument vom Typ (nil :: b) ist der konstante Wert aus dem Zielbereich, der den Rekursionsabbruch darstellt.

foldr kann man also als Rekursionsprinzip auf Listen auffassen.
-}
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' cons nil [] = nil
foldr' cons nil (x : xs) = cons x $ foldr' cons nil xs

{-
Um mittels foldr rekursive Funktionen auf Listen zu definieren, müssen wir also zwei "Fälle" als Argumente übergeben.
Den Fall der nicht leeren Liste (cons:: a -> b -> b), der ein Element der Liste und die Rückgabe des rekursiven Aufrufs 
in ein Element des Zielbereichs übersetzt.
Und den Fall der leeren Liste (nil :: b), der ein konstantes Element des Zielbereichs ist.
-}

{-
Schauen wir uns noch einmal die Funktion len an:
len :: [a] -> Int
len [] = 0
len (_ : tl) = 1 + len tl

Die Zeile 
len (_ : tl) = 1 + len tl
übersetzen wir in das erste Argument von foldr
(\_ r -> 1 + r).
Die Zeile
len [] = 0
übersetzen wir in das zweite Argument von foldr
0.
Nun können wir len mittels foldr definieren:
-}
len' = foldr' (\_ r -> 1 + r) 0 

{-
Der einzig "schwierige" Schritt war also
aus
len (_ : tl) = 1 + len tl
den Ausdruck 
(\_ r -> 1 + r)
abzuleiten.

Man kann das in etwa so erklären:
Da Pattern Matching nichts über Rekursion sagt und lediglich
das Argument "zerlegt", müssen wir bei einer rekursiven Definition
mittels Pattern Matching auch explizit den rekursiven Aufruf angeben.
Die Idee von foldr, wie wir sie oben eingeführt haben, ist ja die 
rekursive Struktur der Argumentliste als Rekursionsprinzip zu nutzen.
In einer Definition mittels foldr müssen wir also keinen rekursiven Aufruf 
auf der Argumentliste angeben, sondern sagen, wie wir mit dem Ergebnis des
rekursiven Aufruf umgehen.
-}

{-
Die Definition foldr' erinnert zurecht an die Definition von applyNtimes.
Doch bei applyNtimes konnten wir den rekursiven Aufruf an unterschiedlichen Stellen 
implementieren.
Wenn wir das bei der Definition von foldr machen, erhalten wir die ebenfalls aus der Vorlesung
bekannte Funktion foldl:
-}
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' fons nil [] = nil
foldl' fons nil (x : xs) = foldl' fons (fons nil x) xs

{-
Für Interessierte ein paar weitergehende Anmerkungen:
foldr ist das allgemeinere Prinzip.
foldl ist sozusagen die "tail-rekursive" Variante von foldr.
Wir können foldl mittels foldr definieren, aber nicht anders herum.
-}