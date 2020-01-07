module Game where

import Control.Monad(forM_, replicateM_,when,unless)
import RNG
import Field
import State


--------------------------------------------------------------------------------
-- View

viewDrawPile :: Action [Card]
viewDrawPile =
  do cs <- get theDrawPile
     randomView (shuffle cs)




--------------------------------------------------------------------------------

-- | Shuffle in the discarded in the draw pile.
reshuffle :: Action ()
reshuffle =
  do draw    <- get theDrawPile
     discard <- get theDiscarded
     set theDiscarded []
     set theDrawPile (draw ++ discard)
     shuffleDraw

-- | Shuffle the draw pile.
-- Usually happens after a reshuffle, but also at the start of a battle.
shuffleDraw :: Action ()
shuffleDraw =
  do draw <- get theDrawPile
     set theDrawPile =<< random (shuffle draw)

-- | Move the top card of the draw deck (if any) to the hand.
-- Reshuffles the discard pile, if neccessary.
drawNextCard :: Action ()
drawNextCard =
  drawOr (reshuffle *> drawOr (pure ()))
  where
  drawOr k =
    do mb <- removeFrom theDrawPile 0
       case mb of
          Just c  -> drawCard c
          Nothing -> k

-- | Assumes card is not in any pile
drawCard :: Card -> Action ()
drawCard c =
  do doEvent c whenDrawn
     addTo theHand c

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


--------------------------------------------------------------------------------




endOfRound :: Action ()
endOfRound =
  do update (entity player ~> entityAttrs) attrEndOfRound
     es <- get enemies
     forM_ es \e -> update (entity e ~> entityAttrs) attrEndOfRound

entityStartTurn :: Entity -> Action ()
entityStartTurn e =
  update (entity e ~> entityAttrs) (removeAttribute Block)

startPlayerTurn :: Action ()
startPlayerTurn =
  do update turn (+1)
     entityStartTurn player
     replicateM_ 5 drawNextCard

enemyTurn :: Action ()
enemyTurn =
  do es <- get enemies
     forM_ es entityStartTurn
     forM_ es doEnemyAction

doEnemyAction :: Entity -> Action ()
doEnemyAction e =
  do living <- isAlive e
     when living
      do EnemyTurn _ act <- get (enemy e ~> enemyAI)
         next <- act
         set (enemy e ~> enemyAI) next




--------------------------------------------------------------------------------


targetDied :: Entity -> Action ()
targetDied _ = pure () -- XXX


actualAttackAmount :: Entity -> Entity -> Int -> Action Int
actualAttackAmount src tgt amt =
  do sas <- get (entity src ~> entityAttrs)
     let amt1 = amt + getAttribute Strength sas
     tas  <- get (entity tgt ~> entityAttrs)
     let amt2 = if getAttribute Vulnerable tas > 0
                   then div (3 * amt1) 2
                   else amt1
     pure amt2



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


removeDead :: Action ()
removeDead =
  do es <- get enemies
     forM_ es \e ->
        do living <- isAlive e
           unless living (removeEnemy e)

isAlive :: Entity -> Action Bool
isAlive e =
  do as <- get (entity e ~> entityAttrs)
     let health = getAttribute Health as
     pure (health > 0)


--------------------------------------------------------------------------------

gainBlock :: Entity -> Int -> Action ()
gainBlock e n =
  update (entity e ~> entityAttrs) (updateAttribute Block n)

gainDebuff :: Entity -> Int -> Attribute -> Action ()
gainDebuff e n a =
  update (entity e ~> entityAttrs) (updateAttribute a n)

gainBuff :: Entity -> Int -> Attribute -> Action ()
gainBuff e n a =
  update (entity e ~> entityAttrs) (updateAttribute a n)

heal :: Entity -> Int -> Action ()
heal e n =
  do yes <- isAlive e
     when yes (update (entity e ~> entityAttrs) (updateAttribute Health n))



--------------------------------------------------------------------------------
playCardFromHand :: Card -> Entity -> Action ()
playCardFromHand c tgt =
  do yes <- doEvent1 c isPlayable tgt
     when yes
       do removeCardFrom theHand c
          doEvent1 c whenPlay tgt
          doEvent c afterPlay
          removeDead

endPlayerTurn :: Action ()
endPlayerTurn =
  do cs <- get theHand
     set theHand []
     forM_ cs \c -> doEvent c atEndOfTurn
     enemyTurn
     endOfRound
     startPlayerTurn


