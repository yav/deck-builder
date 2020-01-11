module Game where

import Control.Monad(forM_, replicateM_,when,unless)
import RNG
import Field
import State

import Debug.Trace


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
  set (entity e ~> entityAttrs ~> attribute Block) 0

startPlayerTurn :: Action ()
startPlayerTurn =
  do update turn (+1)
     entityStartTurn player
     let my x = entity player ~> entityAttrs ~> attribute x
     set (my Energy) =<< get (my MaxEnergy)
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
  do str <- get (entity src ~> entityAttrs ~> attribute Strength)
     vul <- get (entity tgt ~> entityAttrs ~> attribute Vulnerable)
     let mul = if vul > 0 then 1.5 :: Rational else 1
     pure (floor (mul * fromIntegral (amt + str)))



attack :: Entity -> Entity -> Int -> Action Int
attack src tgt amt =
  do yes <- isAlive tgt
     if yes then doDamage tgt =<< actualAttackAmount src tgt amt
            else pure 0

doDamage :: Entity -> Int -> Action Int
doDamage tgt amt =
  do blocked <- reduceNonNeg (entity tgt ~> entityAttrs ~> attribute Block) amt
     let unblocked = amt - blocked
     if unblocked > 0
        then reduceHP tgt unblocked
        else pure 0


reduceHP :: Entity -> Int -> Action Int
reduceHP tgt n =
  do amt <- reduceNonNeg (entity tgt ~> entityAttrs ~> attribute Health) n
     when (amt <= n) (targetDied tgt)
     pure amt


removeDead :: Action ()
removeDead =
  do es <- get enemies
     forM_ es \e ->
        do living <- isAlive e
           unless living (removeEnemy e)

isAlive :: Entity -> Action Bool
isAlive e = (> 0) <$> get (entity e ~> entityAttrs ~> attribute Health)


--------------------------------------------------------------------------------

gainBlock :: Entity -> Int -> Action ()
gainBlock e n =
  update (entity e ~> entityAttrs ~> attribute Block) (+n)

gainDebuff :: Entity -> Int -> EntAttr -> Action ()
gainDebuff e n a =
  update (entity e ~> entityAttrs ~> attribute a) (+n)

gainBuff :: Entity -> Int -> EntAttr -> Action ()
gainBuff e n a =
  update (entity e ~> entityAttrs ~> attribute a) (+n)

heal :: Entity -> Int -> Action ()
heal e n =
  do yes <- isAlive e
     when yes (update (entity e ~> entityAttrs ~> attribute Health) (+n))



--------------------------------------------------------------------------------
playCardFromHand :: Card -> Entity -> Action ()
playCardFromHand c tgt =
  do yes <- doEvent1 c isPlayable tgt
     unless yes abort

     cost <- cardCost c

     () <- traceM ("****************" ++ show cost)
     case cost of
       CostAll -> pure () -- X cards modify energy (so they can see)
       Cost n  ->
         do let energy = entity player ~> entityAttrs ~> attribute Energy
            paid <- reduceNonNeg energy n
            when (paid < n) abort

     removeCardFrom theHand c
     doEvent1 c whenPlay tgt
     doEvent c afterPlay
     removeDead

cardCost :: Card -> Action CardCost
cardCost c =
  do cs <- get (cardAttrs c)
     if hasAttribute TurnCost cs
        then pure (Cost (getField (attribute TurnCost) cs))
        else normalCost c


endPlayerTurn :: Action ()
endPlayerTurn =
  do cs <- get theHand
     set theHand []
     forM_ cs \c -> doEvent c atEndOfTurn
     enemyTurn
     endOfRound
     startPlayerTurn


