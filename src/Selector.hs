module Selector where

newtype Selector a = Selector [(a,Selected)]

data Selected = Selected | NotSelected

newSelector :: [a] -> Selector a
newSelector xs = Selector [ (x,NotSelected) | x <- xs ]

select :: Int -> Selected -> Selector a -> Selector a
select n x (Selector xs) =
  case splitAt n xs of
    (as,(b,_) : bs) -> Selector (as ++ (b,x) : bs)
    _               -> Selector xs

selected :: Selector a -> [a]
selected (Selector xs) = [ a | (a,Selected) <- xs ]


