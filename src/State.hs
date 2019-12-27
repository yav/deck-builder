{-# Language BlockArguments #-}
module State where

import RNG

data Card = Card
  {
  }

data State = State
  { drawPile     :: [Card]
  , hand         :: [Card]
  , discarded    :: [Card]
  , exhausted    :: [Card]
  , selected     :: Maybe Card
  , rng          :: RNG
  }



--------------------------------------------------------------------------------
data Pile = Pile { getCardsFrom :: State -> [Card]
                 , setCardsIn   :: [Card] -> State -> State
                 }

theDrawPile :: Pile
theDrawPile = Pile { getCardsFrom = drawPile
                   , setCardsIn   = \cs s -> s { drawPile = cs }
                   }

theDiscarded :: Pile
theDiscarded = Pile { getCardsFrom = discarded
                    , setCardsIn   = \cs s -> s { discarded = cs }
                    }


theExhausted :: Pile
theExhausted = Pile { getCardsFrom = exhausted
                    , setCardsIn   = \cs s -> s { exhausted = cs }
                    }

theHand :: Pile
theHand = Pile { getCardsFrom = hand
               , setCardsIn   = \cs s -> s { hand = cs }
               }
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

-- | Update the state with the given function, but only if something is selected
whenSelected :: (Card -> State -> State) -> State -> State
whenSelected k s =
  case selected s of
    Just a  -> k a s
    Nothing -> s

-- | Update the state with the given function, but only if nothing is selected
whenNotSelected :: (State -> State) -> State -> State
whenNotSelected k s =
  case selected s of
    Just _  -> s
    Nothing -> k s

-- | Clear the selection, do an update, and then restore the selection
-- to what it was originallt.
saveSelected :: (State -> State) -> State -> State
saveSelected k s =
  let it = selected s
      s1 = k s { selected = Nothing }
  in s1 { selected = it }

--------------------------------------------------------------------------------



-- | Shuffle in the discarded in the draw pile.
reshuffle :: State -> State
reshuffle s =
  withRNG (rng s)
  do newDraw <- shuffle (drawPile s ++ discarded s)
     pure \r -> s { drawPile = newDraw, discarded = [], rng = r }


-- | Move the top card of the draw deck (if any) to the hand.
-- Reshuffles the discard pile, if neccessary.
drawCard :: State -> State
drawCard = saveSelected (whenNotSelected (draw . reshuffle) . draw)
  where draw = selectFrom theDrawPile 0


-- | Select a card at a specific location in a draw pile.
selectFrom :: Pile -> Int -> State -> State
selectFrom p n s =
  case removeAt n (getCardsFrom p s) of
    Just (a,ys) -> setCardsIn p ys s { selected = Just a }
    Nothing     -> s



--------------------------------------------------------------------------------

-- | Move the selected card to the top of the given pile.
selectedTo :: Pile -> State -> State
selectedTo p =
  whenSelected \a s ->
  setCardsIn p (a : getCardsFrom p s) s { selected = Nothing }


-- | Move the selected card to the bottom of the draw deck.
selectedToDrawBottom :: State -> State
selectedToDrawBottom =
  whenSelected \a s ->
  s { drawPile = drawPile s ++ [a], selected = Nothing }

-- | Shuffle the selected card in the draw deck.
selectedToDrawRandom :: State -> State
selectedToDrawRandom =
  whenSelected \a s ->
  withRNG (rng s)
  do let cs = drawPile s
     n <- randInRange 0 (length cs + 1)
     pure \r -> s { drawPile = insertAt n a cs
                  , selected = Nothing
                  , rng      = r
                  }
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
insertAt :: Int -> a -> [a] -> [a]
insertAt n a xs = case splitAt n xs of
                    (front, back) -> front ++ a : back

removeAt :: Int -> [a] -> Maybe (a,[a])
removeAt n xs = case splitAt n xs of
                  (front, a : back) -> Just (a, front ++ back)
                  _                 -> Nothing


