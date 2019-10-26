{-# Language RecordWildCards, BlockArguments #-}
module Field where

a :: Int
a = 2

type FieldOf s a = s -> (a, a -> s)

get :: FieldOf s a -> s -> a
get f s = fst (f s)

set :: FieldOf s a -> a -> s -> s
set f a s = snd (f s) a

upd :: FieldOf s a -> (a -> a) -> s -> s
upd fi f s = set fi (f (get fi s)) s

upd' :: FieldOf s a -> (a -> (b,a)) -> s -> (b,s)
upd' fi f s = (r, set fi a' s)
  where (r,a') = f (get fi s)


el :: Int -> FieldOf [a] a
el n xs =
  case splitAt n xs of
    (as, b : bs) -> (b, \x -> as ++ x : bs)
    _            -> error "el: index out of bounds"



extract :: Int -> [a] -> (a,[a])
extract n xs =
  case splitAt n xs of
    (as, b : bs) -> (b, as ++ bs)
    _            -> error "extract: index out of bounds"

(~>) :: FieldOf s a -> FieldOf a b -> FieldOf s b
(x ~> y) s = (get y tmp, \a -> set x (set y a tmp) s)
  where tmp = get x s


