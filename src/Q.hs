module Q (Q, emptyQ, enQ, enQs, deQ, qFromList, qToList) where

data Q a = Q [a] [a]

emptyQ :: Q a
emptyQ = Q [] []

enQ :: a -> Q a -> Q a
enQ a (Q xs ys) = Q xs (a:ys)

-- | Front of list gets in queue first
enQs :: [a] -> Q a -> Q a
enQs as (Q xs ys) = Q xs (reverse as ++ ys)

deQ :: Q a -> Maybe (a, Q a)
deQ (Q xs ys) =
  case xs of
    x : more -> Just (x, Q more ys)
    [] -> case reverse ys of
            [] -> Nothing
            x : more -> Just (x, Q more [])

qFromList :: [a] -> Q a
qFromList xs = Q xs []

qToList :: Q a -> [a]
qToList (Q xs ys) =
  case ys of
    [] -> xs
    _  -> xs ++ reverse ys

