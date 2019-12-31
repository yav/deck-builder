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
     set theDiscarded []
     set theDrawPile (draw ++ discard)
     shuffleDraw

shuffleDraw :: Action ()
shuffleDraw =
  do draw <- get theDrawPile
     set theDrawPile =<< random (shuffle draw)

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
  do updAttrs (entity player ~> entityAttrs)
     es <- getEnemies
     forM_ es \e -> updAttrs (entity e ~> entityAttrs)
  where
  updAttrs f =
    do update f (removeAttribute Block)   -- XXX: this should happen at the *start* of the turn
       update f attrEndOfRound


startPlayerTurn :: Action ()
startPlayerTurn =
  do update turn (+1)
     replicateM_ 5 drawCard

enemyTurn :: Action ()
enemyTurn =
  do EnemyTurn m <- get theAI
     set theAI =<< m



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


targetDied :: Entity -> Action ()
targetDied e =
  update (entity e ~> entityAttrs) (updateAttribute Dead 1)


actualAttackAmount :: Entity -> Entity -> Int -> Action Int
actualAttackAmount src tgt amt =
  do _sas <- get (entity src ~> entityAttrs)
     tas  <- get (entity tgt ~> entityAttrs)
     if getAttribute Vulnerable tas > 0
        then pure (div (3 * amt) 2)
        else pure amt


attack :: Entity -> Entity -> Int -> Action Int
attack src tgt amt =
  do let who = entity tgt ~> entityAttrs
     as <- get who
     if getAttribute Health as == 0
        then pure 0
        else doDamage tgt =<< actualAttackAmount src tgt amt

doDamage :: Entity -> Int -> Action Int
doDamage tgt amt =
  do let who = entity tgt ~> entityAttrs
     as <- get who
     let (blocked,as1) = reduceNonNeg Block amt as
     set who as1

     let unblocked = amt - blocked
     if unblocked > 0
        then reduceHP tgt unblocked
        else pure 0


reduceHP :: Entity -> Int -> Action Int
reduceHP tgt n =
  do let who = entity tgt ~> entityAttrs
     as <- get who
     let (amt,as1) = reduceNonNeg Health n as
     set who as1
     when (getAttribute Health as1 == 0) (targetDied tgt)
     pure amt



--------------------------------------------------------------------------------
playCardFromHand :: Card -> Entity -> Action ()
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


