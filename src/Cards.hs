module Cards where

import Control.Monad(replicateM_,void)

import Field
import State
import Game


type CardSpec = Card -> CardEvents
type CardProp = CardEvents -> CardEvents


--------------------------------------------------------------------------------
named :: String -> CardProp
named x ev = ev { cardName = x }

cost :: Int -> CardProp
cost x ev = ev { cardNormalCost = Cost x }

costAll :: CardProp
costAll ev = ev { cardNormalCost = CostAll }

withAction :: (Entity -> Action ()) -> CardProp
withAction f ev = ev { whenPlay = \tgt -> f tgt >> whenPlay ev tgt }


ethereal :: CardProp
ethereal = \ev -> ev { atEndOfTurn = exhaustCard (self ev) }

retain :: CardProp
retain = \ev -> ev { atEndOfTurn = retainCard (self ev) }

power :: CardProp
power = \ev -> ev { afterPlay = pure () }

exhausting :: CardProp
exhausting = \ev -> ev { afterPlay = exhaustCard (self ev) }

targeted :: CardProp
targeted = \ev -> ev { isPlayable = \tgt -> if tgt == player
                                              then pure False
                                              else isAlive tgt
                     }

targetAll :: CardProp
targetAll = \ev -> ev { whenPlay = \_ -> mapM_ (whenPlay ev) =<< get enemies }

repeated :: Int -> CardProp
repeated n = \ev -> ev { whenPlay = replicateM_ n . whenPlay ev }

attacking :: Int -> CardProp
attacking n = withAction \tgt -> void (attack player tgt n)

blocking :: Int -> CardProp
blocking n = withAction \_ -> gainBlock player n

debuffing :: Int -> EntAttr -> CardProp
debuffing n a = withAction \tgt -> gainDebuff tgt n a

--------------------------------------------------------------------------------


card :: CardSpec
card c = CardEvents
  { atEndOfTurn     = addTo theDiscarded c
  , afterPlay       = addTo theDiscarded c
  , whenPlay        = \_ -> pure ()
  , whenDrawn       = pure ()
  , whenExhausted   = pure ()
  , whenRetained    = pure ()
  , whenDiscarded   = pure ()
  , isPlayable      = \_ -> pure True
  , self            = c
  , cardName        = ""
  , cardNormalCost  = Cost 0
  }


strike :: CardSpec
strike = named "Strike" . cost 1 . targeted . attacking 6 . card

strike' :: CardSpec
strike' = named "Strike+" . cost 1 . targeted . attacking 9 . card

defend :: CardSpec
defend = named "Defend" . cost 1 . blocking 5 . card

defend' :: CardSpec
defend' = named "Defend+" . cost 1 . blocking 8 . card

bash :: CardSpec
bash = named "Bash" . cost 2
     . targeted . attacking 8 . debuffing 2 Vulnerable . card

bash' :: CardSpec
bash' = named "Bash+" . cost 2
      . targeted . attacking 10 . debuffing 3 Vulnerable . card

spotWeakness :: CardSpec
spotWeakness = named "Spot Weakness" . cost 1 . targeted . withAction act . card
  where
  act tgt = do EnemyTurn is _ <- get (enemy tgt ~> enemyAI)
               case lookup WillAttack is of
                 Just _ -> gainBuff player 3 Strength
                 _ -> pure ()


seek :: CardSpec
seek = named "Seek" . cost 0 . exhausting . withAction seekAct . card
  where
  seekAct _ =
    do d  <- viewDrawPile
       cs <- choose "Choose a card to add to the hand" (Exactly 1) d
       case cs of
         c : _ -> do removeCardFrom theDrawPile c
                     drawCard c
         [] -> pure ()

reaper :: CardSpec
reaper = named "Reaper" . exhausting . withAction act . card
  where
  act _ = do es <- get enemies
             total <- sum <$> mapM (\e -> attack player e 4) es
             heal player total
