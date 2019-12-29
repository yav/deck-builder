module Game where

import Control.Monad(forM_, replicateM_)
import RNG
import Field
import State






--------------------------------------------------------------------------------

-- | Shuffle in the discarded in the draw pile.
reshuffle :: Action ()
reshuffle =
  do draw    <- get theDrawPile
     discard <- get theDiscarded
     set theDrawPile =<< random (shuffle (draw ++ discard))
     set theDiscarded []

-- | Move the top card of the draw deck (if any) to the hand.
-- Reshuffles the discard pile, if neccessary.
drawCard :: Action ()
drawCard =
  drawOr (reshuffle *> drawOr (pure ()))
  where
  drawOr k =
    do mb <- removeFrom theDrawPile 0
       case mb of
          Just c  -> addTo theHand c
          Nothing -> k

endOfRound :: Action ()
endOfRound =
  do updAttrs player
     es <- getEnemies
     forM_ es \e -> updAttrs (enemy e ~> enemyAttrs)
  where
  updAttrs f =
    do update f (removeAttribute Block)
       update f attrEndOfRound


startPlayerTurn :: Action ()
startPlayerTurn =
  do replicateM_ 5 drawCard

endPlayerTurn :: Action ()
endPlayerTurn =
  do cs <- get theHand
     set theHand []
     forM_ cs \c -> doEvent c atEndOfTurn

enemyTurn :: Action ()
enemyTurn = pure () --- XXX

--------------------------------------------------------------------------------
exhaustCard :: Card -> Action ()
exhaustCard c =
  do doEvent c whenExhausted
     addTo theExhausted c


retainCard :: Card -> Action ()
retainCard c =
  do doEvent c whenRetained
     addTo theHand c

discardCard :: Card -> Action ()
discardCard c =
  do doEvent c whenDiscarded
     addTo theDiscarded c


-- | Play this card.
-- Assumes the card is not in any of the piles.
playCard :: Card -> Action ()
playCard c = undefined

{-
  do yes <- doEvent c playCard
     when yes
       do doEvent c afterPlay

playCardOn :: Card -> Enemy -> Action ()
playCardOn = undefined
-}


