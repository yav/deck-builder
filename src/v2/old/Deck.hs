{-# Language RecordWildCards, BlockArguments #-}
module Deck where

import RNG
import Field

data Deck a = Deck
  { drawPile    :: [a]
  , discardPile :: [a]
  , rng         :: RNG
  } deriving Show

emptyDeck :: RNG -> Deck a
emptyDeck  r = Deck { drawPile = []
                    , discardPile = []
                    , rng = r }


type Pile a = FieldOf (Deck a) [a]

theDiscardPile :: Pile a
theDiscardPile s = (discardPile s, \new -> s { discardPile = new })

theDrawPile :: Pile a
theDrawPile s = (drawPile s, \new -> s { drawPile = new })


--------------------------------------------------------------------------------

draw :: Deck a -> Maybe (a, Deck a)
draw Deck { .. } =
  case drawPile of
    x : xs -> Just (x, Deck { drawPile = xs, .. })
    [] -> genRandFun rng
          do cs <- shuffle discardPile
             pure \r -> case cs of
                          []     -> Nothing
                          x : xs ->
                            Just (x, Deck { drawPile = xs
                                          , discardPile = []
                                          , rng = r })

drawMany :: Int -> Deck a -> ([a], Deck a)
drawMany n d
  | n > 0, Just (a,d1) <- draw d = let (rest,d2) = drawMany (n-1) d1
                                   in (a:rest,d2)
  | otherwise = ([], d)


addTo :: Pile a -> a -> Deck a -> Deck a
addTo p a = upd p (a:)

addManyTo :: Pile a -> [a] -> Deck a -> Deck a
addManyTo p as = upd p (as++)





