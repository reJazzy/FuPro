module VL7 where

{-
Lesermonade ((->) a)
-}

data Date = Date {
    day :: Int,
    month :: Int,
    year :: Int
    } deriving Show

today :: Date
today = Date 30 5 2025

haskellBirthday :: Date
haskellBirthday = Date 12 9 1900

data Person = Person {
    name :: String,
    birthday :: Date
    }

haskellBcurry :: Person
haskellBcurry = Person "Haskell B. Curry" haskellBirthday

{-
Mit let
-}
age :: (Date, Person) -> Int
age tuple =
    let birthDate = birthday $ snd tuple
        birthYear = year birthDate
        todayYear = year $ fst tuple
    in birthYear - todayYear

testAge :: Int
testAge = curry age today haskellBcurry

{-
Mit der Lesermonade
-}
ageM :: (Date, Person) -> Int
ageM = do
    birthYear <- year . birthday . snd
    todayYear <- year . fst
    return $ birthYear - todayYear

testAgeM :: Int
testAgeM = curry ageM today haskellBcurry

testReader :: Bool
testReader = testAge == testAgeM

{-
Schreibermonade ((,) a)
-}

logging :: String -> (String, ())
logging s = (s, ())

computationWithLog :: Int -> Int -> (String, Maybe Int)
computationWithLog n m = do 
    logging "Starte Berechnung: \n"
    if m == 0 
    then do 
        logging "Fehler! Wir dürfen nicht durch 0 teilen!" 
        return Nothing 
    else do 
        logging "Berechnung erfolgreich."
        return $ return $ div n m

testWriter1 :: (String, Maybe Int)
testWriter1 = computationWithLog 42 0 

testWriter2 :: (String, Maybe Int)
testWriter2 = computationWithLog 42 7 

{-
Zustandsmonade
-}

data Zustand = Gesperrt | Frei
  deriving (Eq, Show)

data Ausgabe = Danke | Offen | Piep
  deriving (Eq, Show)

bezahlen :: Zustand -> (Ausgabe, Zustand)
bezahlen _ = (Danke, Frei)

drücken :: Zustand -> (Ausgabe, Zustand)
drücken Gesperrt = (Piep , Gesperrt)
drücken Frei = (Offen, Gesperrt)

{-
Komposition mittels let
-}
dau :: Zustand -> ([Ausgabe], Zustand)
dau s0 = let (a1, s1) = drücken s0
             (a2, s2) = drücken s1 
             (a3, s3) = drücken s2 
             (a4, s4) = bezahlen s3 
             (a5, s5) = bezahlen s4 
             (a6, s6) = drücken s5 
         in ([a1, a2, a3, a4, a5, a6], s6)

testDau :: ([Ausgabe], Zustand)
testDau = dau Gesperrt

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
Die Zustandsbehafteten Funktionen bezahlen und drücken können einfach mittels des 
State-Konstruktors in die Zustandsmonade eingebettet werden.
-}
bezahlenM :: State Zustand Ausgabe 
bezahlenM = State bezahlen

drückenM :: State Zustand Ausgabe 
drückenM = State drücken 

{-
Komposition mit der Zustandsmonade
-}
dauM :: State Zustand [Ausgabe]
dauM = do 
    a1 <- drückenM
    a2 <- drückenM
    a3 <- drückenM
    a4 <- bezahlenM 
    a5 <- bezahlenM 
    a6 <- drückenM
    return [a1, a2, a3, a4, a5, a6]

testDauM :: ([Ausgabe], Zustand) 
testDauM = runS dauM Gesperrt

testState :: Bool 
testState = testDau == testDauM