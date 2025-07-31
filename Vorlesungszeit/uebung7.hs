module Uebung7 where

{-
Die Funktion guard aus Control.Monad ist leider nicht im Prelude definiert, darf aber
in der Übung, im Midterm-Test und der Klausur vorausgesetzt werden,
sofern die konkrete Aufgabenstellung dies nicht ausschließt.
-}
import Control.Monad ( guard )
import Text.ParserCombinators.ReadP (look, get)

{-
Übung zur Vorlesung
Funktionale Programmierung
Sommersemester 2025
Übungsblatt 7
-}

{-
Ausgabe: 30.05.2025
Abgabe: kein
-}

{-
Aufgabe 7.1 - Monaden anwenden
-}

{-
a)
Implementieren Sie eine Funktion
mapFi :: (a -> b) -> (b -> Bool) -> [a] -> [b],
die zuerst die übergebene Funktion über die Liste mapped und 
dann das Resultat nach dem übergebenen Prädikat filtert.
Nutzen Sie zur Implementierung nur die (>>=)-Notation.
-}            -- verdoppeln             -- ist gerade?
xs = [1, 2, 3, 4, 5]

mapFi :: (a -> b) -> (b -> Bool) -> [a] -> [b]
mapFi f p xs =
  xs >>= \x -> if p (f x) then [f x] else []


{-
b)
Implementieren Sie die Funktion mapFi aus 7.1 a) erneut mittels Do-Notation.
(Bzw. übersetzen Sie die (>>=)-Notation in die Do-Notation.)
Nennen Sie die Funktion
mapFiDo :: (a -> b) -> (b -> Bool) -> [a] -> [b].
-}

mapFiDo :: (a -> b) -> (b -> Bool) -> [a] -> [b]
mapFiDo f p xs = do
  x <- xs
  guard(p (f x))
  [f x]


{-
c)
Implementieren Sie eine Funktion
zipApp :: [a -> Maybe b] -> [a] -> Either String [b],
die eine Liste von Funktionen mit einer Liste von Argumenten "zipped" 
(paarweise je Index appliziert, also ähnlich zu zip und nicht (<*>)) 
und auftretende Fehler in der Either-Monade abhandelt.
Sollten die Listen nicht gleichlang sein, soll entweder der Fehler
"Linke Liste zu kurz."
oder
"Rechte Liste zu kurz."
zurückgegeben werden.
Sollte bei einer Funktionsapplikation Nothing als Ergebnis
herauskommen, soll der Fehler
"Nothing bei Funktionsanwendung."
zurückgegeben werden.
Ansonsten sollen die Funktionen aus der ersten Liste jeweils auf das Element
mit demselben Index in der zweiten Liste angewendet werden und das Ergebnis
in der Either-Monade, statt der Maybe-Monade zurückgegeben werden.
-}

zipApp :: [a -> Maybe b] -> [a] -> Either String [b]
zipApp [] [] = Right []
zipApp [] as = Left "Linke Liste zu kurz"
zipApp fs [] = Left "Rechte Liste zu kurz"
zipApp (f:fs) (a:as) =
  case f a of 
    Nothing -> Left "Noting bei Funktionsanwendung"
    Just b -> case zipApp fs as of
      Left x -> Left x
      Right x -> Right (b : x)

zipApp' :: [a -> Maybe b] -> [a] -> Either String [b]
zipApp' [] [] = Right []
zipApp' [] as = Left "Linke Liste zu kurz"
zipApp' fs [] = Left "Rechte Liste zu kurz"
zipApp' (f:fs) (a:as) =
  case f a of 
    Nothing -> Left "Noting bei Funktionsanwendung"
    Just b -> zipApp' fs as >>= \x -> Right (b : x)


{-
Aufgabe 7.2 - Eine funktionale Bank
-}

{-
Die Definitionen der Leser-, Schreiber- und Zustandsmonade finden sich in
FuPro_2025_VL7.pdf.
-}

{-
Gegeben seien die folgenden Datentypen zur Modellierung von (Bank-)Konten und
(bankinternen) Überweisungen zwischen Kontoinhabern.
-}

type ID = Int

type Bank = [(ID,Account)]

data Account = Account { balance :: Int, owner :: Client } deriving (Show, Eq)

data Client = Client
  { name :: String
  , surname :: String
  , address :: String
  } deriving (Show, Eq)

own1, own2, own3 :: Client
own1 = Client "Max" "Mustermann" "Musterhausen"
own2 = Client "John" "Doe" "Somewhere"
own3 = Client "Erika" "Mustermann" "Musterhausen"

acc1, acc2, acc3 :: Account
acc1 = Account 100 own1
acc2 = Account 0 own2
acc3 = Account 50 own3

bank1 :: Bank
bank1 = [(1,acc1), (2,acc2), (3,acc3)]

updRel :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
updRel ((a,b):r) c d = if a == c then (a,d):r else (a,b):updRel r c d 
updRel _ a b = [(a,b)]

putAccount :: ID -> Bank -> Account -> Bank
putAccount id bank acc = updRel bank id acc

getAccount :: ID -> Bank -> Maybe Account
getAccount id bank = lookup id bank

credit :: Int -> ID -> Bank -> Bank
credit n i b = putAccount i b entry{ balance = oldBalance + n } 
  where
  Just entry = getAccount i b
  oldBalance = balance entry

debit :: Int -> ID -> Bank -> Bank
debit n = credit (-n)

transfer :: Int -> ID -> ID -> Bank -> Bank
transfer amount id1 id2 bank = let b = credit amount id2 bank in debit amount id1 b

{-
Wenn wir uns die Typen von
putAccount :: ID -> Bank -> Account -> Bank,
getAccount :: ID -> Bank -> Maybe Account,
credit :: Int -> ID -> Bank -> Bank,
debit :: Int -> ID -> Bank -> Bank
und
transfer :: Int -> ID -> ID -> Bank -> Bank
anschauen, dann stellen wir fest, dass alle Funktion auf derselben
"Umgebung" aus ID und Bank arbeiten.
Sprich alle Funktionen haben mindestens ein Argument vom Typ ID und vom Typ Bank.

Das liegt daran, dass diese Funktionen immer in Bezug zu einem Konto
auf einer Bank arbeiten.

Die Lesermonade ermöglicht es uns Funktionen "in" dieser Umgebung zu
definieren, sprich die Umgebung als implizit gegeben zu behandeln.

Hierzu führen wir einen Typen für unsere Umgebung aus Kontonummer und Bank ein:
-}

data Environment = Env {
                        idB :: ID,
                        bank :: Bank
                       } deriving (Show, Eq)

{-
a)
Nutzen Sie die Lesermonade ((->) Environment) um die Funktionen
putAccountR :: Account -> (->) Environment Bank,
getAccountR :: (->) Environment (Maybe Account),
creditR  :: Int -> (->) Environment Bank,
debitR :: Int -> (->) Environment Bank
und
transferR :: Int -> ID -> (->) Environment Bank
analog zu den zuvor definierten Funktionen zu implementieren.

Die obigen Funktionen nutzen den Funktionspfeil zur 
Verdeutlichung der Lesermonade in Prefix-Schreibweise. 
Diese Typen sind natürlich äquivalent zu den folgenden:
putAccountR :: Account -> Environment -> Bank
getAccountR :: Environment -> (Maybe Account)
creditR  :: Int -> Environment -> Bank
debitR :: Int -> Environment -> Bank
transferR :: Int -> ID -> Environment -> Bank

Verwenden Sie die do-Notation zur Definition der Funktionen.
-}

putAccountR :: Account -> (->) Environment Bank 
putAccountR account = idB >>= \id -> bank >>= \bank -> return (updRel bank id account)

putAccountR' :: Account -> (->) Environment Bank 
putAccountR' account = do
  id <- idB
  bank <- bank
  return (updRel bank id account)

getAccountR :: (->) Environment (Maybe Account)
getAccountR = do
  id <- idB
  bank <- bank
  return (lookup id bank)

getAccountR' :: (->) Environment (Maybe Account)
getAccountR' = idB >>= \id -> bank >>= \bank -> return (lookup id bank)

creditR :: Int -> (->) Environment Bank
creditR c = getAccountR >>= \acc -> case acc of
  Just acc -> putAccountR acc{ balance = balance acc + c }
  Nothing -> bank

creditR' :: Int -> (->) Environment Bank
creditR' c = do
  acc <- getAccountR
  case acc of 
    Just acc -> putAccountR acc{ balance = balance acc + c }
    Nothing -> bank

debitR :: Int -> (->) Environment Bank
debitR d = creditR (-d)

-- transfer amount id1 id2 bank = let b = credit amount id2 bank in debit amount id1 b

transferR :: Int -> ID -> (->) Environment Bank
transferR amount fromId = do
  b <- creditR amount
  let env' = Env{idB = fromId, bank = b}
  return (debitR amount env')


{-
Sie können transferR mittels der folgenden Funktion
transactions :: Bank -> Bank
testen.
Folgender Beispielaufruf sollte mit ihrer Implementierung
übereinstimmen:
transactions bank1
~>
[
(1,Account {balance = 25, owner = Client {name = "Max", surname = "Mustermann", address = "Musterhausen"}}),
(2,Account {balance = 25, owner = Client {name = "John", surname = "Doe", address = "Somewhere"}}),
(3,Account {balance = 100, owner = Client {name = "Erika", surname = "Mustermann", address = "Musterhausen"}})
]
-}

transactions bank = transferR 25 2 $ Env 3 $ transferR 25 1 $ Env 3 $ transferR 50 1 $ Env 2 bank


{-
b)
Nutzen Sie die Schreibermonade ((,) String) um eine Funktion
transferLog :: Int -> ID -> ID -> Bank -> (,) String Bank
zu implementieren, die den angegebenen Betrag vom ersten Konto auf das zweite überweist
und einen Eintrag in das Protokoll schreibt. Der Eintrag soll folgendes Format besitzen:

"Der Betrag <amount> wurde von Konto <id1> auf Konto <id2> übertragen."

Wie bei der Lesermonade, haben wir zur Verdeutlichung der Schreibermonade im Typ 
von transferLog den Tupelkonstruktor in Prefix-Schreibweise appliziert. Der obige Typ
ist natürlich äquivalent zu folgender Schreibweise:
transferLog :: Int -> ID -> ID -> Bank -> (String, Bank)

Verwenden Sie die do-Notation. Der Tupelkonstruktor (,) soll nicht benutzt werden!

Tipp: Es lohnt sich die Loggerfunktion
logging :: String -> (String, ())
aus FuPro_2025_VL7.pdf zu implementieren.
(Für logging darf der Tupelkonstruktor (,) benutzt werden.)
-}

logging :: String -> (String, ())
logging s = (s, ())

transferLog :: Int -> ID -> ID -> Bank -> (,) String Bank
transferLog val id1 id2 bank = do
  logging ("Der Betrag " ++ show val ++ " wurde von Konto " ++ show id1 ++ " auf Konto " ++ show id2 ++ " uebertragen.\n")
  pure (transfer val id1 id2 bank)


{-
Sie können transferLog mittels der folgenden Funktion
transactionsLog :: Bank -> (,) String Bank
testen.
Folgende Beispielaufrufe sollte mit ihrer Implementierung
übereinstimmen:
putStrLn $ fst $ transactionsLog bank1
~>
Der Betrag 50 wurde von Konto 1 auf Konto 2 übertragen.
Der Betrag 25 wurde von Konto 1 auf Konto 3 übertragen.
Der Betrag 25 wurde von Konto 2 auf Konto 3 übertragen.

snd $ transactionsLog bank1
~>
[
(1,Account {balance = 25, owner = Client {name = "Max", surname = "Mustermann", address = "Musterhausen"}}),
(2,Account {balance = 25, owner = Client {name = "John", surname = "Doe", address = "Somewhere"}}),
(3,Account {balance = 100, owner = Client {name = "Erika", surname = "Mustermann", address = "Musterhausen"}})
]
-}

transactionsLog :: Bank -> (,) String Bank
transactionsLog bank = do
  bank2 <- transferLog 50 1 2 bank
  bank3 <- transferLog 25 1 3 bank2
  transferLog 25 2 3 bank3

{-
Da die Zustandsmonade, im Gegensatz zur Leser- und Schreibermonade, nicht
im Prelude ist, definieren wir hier unsere Version entsprechend der Folien.

Die Aufgabe bezieht sich also auf die hier definierte Zustandsmonade:
-}

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
c)
Nutzen Sie die Zustandsmonade (State Bank) um die Funktionen
putAccountS :: ID -> Account -> State Bank (),
getAccountS :: ID -> State Bank (Maybe Account)
creditS :: Int -> ID -> State Bank (),
debitS :: Int -> ID -> State Bank ()
und
transferS :: Int -> ID -> ID -> State Bank ()
analog zu den zuvor definierten Funktionen zu implementieren.

putAccountS und getAccountS sind unsere zustandsbehafteten Funktionen,
die wir mit dem State-Konstruktor in die Zustandsmonade liften.
Alle anderen Funktionen sollen mittels Komposition
von putAccountS und getAccountS definiert werden.

Nutzen Sie zur Implementierung der komponierten Funktionen die do-Notation.

-- updRel bank id acc
-}

putAccountS :: ID -> Account -> State Bank ()
putAccountS id acc = State(\s -> ((), updRel s id acc))

getAccountS :: ID -> State Bank (Maybe Account)
getAccountS id = State(\s -> (lookup id s, s))

creditS :: Int -> ID -> State Bank ()
creditS val id = do
  acc <- getAccountS id
  case acc of
    Just acc -> putAccountS id acc{balance = balance acc + val}
    Nothing -> pure ()

debitS :: Int -> ID -> State Bank ()
debitS val id = creditS (-val) id

-- transfer amount id1 id2 bank = let b = credit amount id2 bank in debit amount id1 b

transferS :: Int -> ID -> ID -> State Bank ()
transferS val id1 id2 = do
  creditS val id2
  debitS val id1

{-
Folgender Beispielaufruf kann zum Verständnis der Aufgabe und Überprüfen ihrer
Lösungen hilfreich sein:

 fmap (fmap balance)$ snd $ runS transactionsS bank1
~>
[(1,25),(2,25),(3,100)]
-}
transactionsS = do
  transferS 50 1 2
  transferS 25 1 3
  transferS 25 2 3

{-
Aufgabe 7.* - reverse
-}

{-
Implementieren Sie die Funktion
reverse' :: [a] -> [a],
die eine gegebene Liste "umdreht" mittels einer Listenfaltung.
-}

--reverse' :: [a] -> [a]
