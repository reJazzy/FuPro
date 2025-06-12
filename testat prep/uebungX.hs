module UebungX where
import Control.Monad (guard)

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


type Queue = [Int]

{-
  enqueue soll ein element in die queue einreihen
-}
enqueue :: Int -> State Queue ()
enqueue val = State(\s -> ((), s ++ [val]))

{-
  dequeue soll das älteste element aus der queue entfernen
-}
dequeue :: State Queue (Maybe Int)
dequeue = State(\s -> if s == [] then (Nothing, s) else (Just (head s), tail s))

{-
  die queue soll reversed werden
   nicht state konstruktor benutzen
-}
reverseQueue :: State Queue ()
reverseQueue = do
    elm <- dequeue
    case elm of
        Nothing -> return ()
        Just elm -> do
            reverseQueue
            enqueue elm

{-
  entferne n elemente aus der queue, wenn n größer ist als die queue lang ist, dann gebe die ganze queue zurück
  nicht state konstruktor benutzen
-}
dequeueN :: Int -> State Queue [Int]
dequeueN 0 = return []
dequeueN n = do
    elm <- dequeue
    case elm of
        Nothing -> return []
        Just elm -> do
            queue <- dequeueN (n-1)
            return (elm : queue)


{-
  entferne n elemente aus der queue. Wenn n größer ist als die queue lang ist, dann gebe nichts zurück, sonst die entfernten elemente
   nicht state konstruktor benutzen
-}
dequeueN' :: Int -> State Queue (Maybe [Int])
dequeueN' 0 = return (Just [])
dequeueN' n = do
    x <- dequeue
    case x of
        Nothing -> return Nothing
        Just val -> do
            rest <- dequeueN' (n - 1)
            case rest of
                Nothing -> return Nothing
                Just vals -> return (Just (val : vals))

{-
  Gegeben sei eine Funktion f. Wende diese funktion auf jedes element in der queue an. Alle elemente, für welche die funktion erfolgreich ist, werden ausgegeben, alle anderen elemente bleiben in der queue
nutze nicht den state konstruktor
  (reihenfolge egal!)
-}
applyN :: (Int -> Maybe a) -> State Queue [a]
applyN f = do
    a <- dequeue
    case a of
        Nothing -> return []
        Just a ->
            case f a of
                Nothing -> do
                    enqueue a
                    applyN f
                Just b -> do
                    as <- applyN f
                    return (b : as) 
                