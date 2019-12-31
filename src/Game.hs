module Game where

import Control.Monad(forM_, replicateM_,when)
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
          Just c  -> cardDrawn c
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
  do update turn (+1)
     replicateM_ 5 drawCard

enemyTurn :: Action ()
enemyTurn = mapM_ doEnemyAction =<< getEnemies

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


-- | Assumes card is not in any pile
cardDrawn :: Card -> Action ()
cardDrawn c =
  do doEvent c whenDrawn
     addTo theHand c


viewDrawPile :: Action [Card]
viewDrawPile =
  do cs <- get theDrawPile
     randomView (shuffle cs)

--------------------------------------------------------------------------------


targetDied :: Target -> Action ()
targetDied _ = pure () -- XXX


reduceHP :: Target -> Int -> Action Int
reduceHP tgt n =
  do as <- get who
     if hasAttribute Health as
       then do let hp    = getAttribute Health
                   newHP = max 0 (hp - n)
                   diff  = hp - newHP
               set who (updateAttribute Health (negate diff))
               when (newHP == 0) (targetDied tgt)
               pure diff
       else pure 0
  where
  who = case tgt of
         NoTaget  -> player
         Target e -> enemy e




--------------------------------------------------------------------------------
playCardFromHand :: Card -> CardTaget -> Action ()
playCardFromHand c tgt =
  do yes <- doEvent1 c isPlayable tgt
     when yes
       do removeCardFrom theHand c
          doEvent1 c whenPlay tgt
          doEvent c afterPlay

endPlayerTurn :: Action ()
endPlayerTurn =
  do cs <- get theHand
     set theHand []
     forM_ cs \c -> doEvent c atEndOfTurn
     enemyTurn
     endOfRound
     startPlayerTurn


