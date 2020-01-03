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

repeated :: Int -> CardProp
repeated n = \ev -> ev { whenPlay = replicateM_ n . whenPlay ev }

attacking :: Int -> CardProp
attacking n = withAction \tgt -> void (attack player tgt n)

blocking :: Int -> CardProp
blocking n = withAction \_ -> gainBlock player n

debuffing :: Int -> Attribute -> CardProp
debuffing n a = withAction \tgt -> gainDebuff tgt n a

--------------------------------------------------------------------------------


card :: CardSpec
card c = CardEvents
  { atEndOfTurn   = addTo theDiscarded c
  , afterPlay     = addTo theDiscarded c
  , whenPlay      = \_ -> pure ()
  , whenDrawn     = pure ()
  , whenExhausted = pure ()
  , whenRetained  = pure ()
  , whenDiscarded = pure ()
  , isPlayable    = \_ -> pure True
  , self          = c
  , cardName      = ""
  }


strike :: CardSpec
strike = named "Strike" . targeted . attacking 6 . card

strike' :: CardSpec
strike' = named "Strike+" . targeted . attacking 9 . card

defend :: CardSpec
defend = named "Defend" . blocking 5 . card

defend' :: CardSpec
defend' = named "Defend+" . blocking 8 . card

bash :: CardSpec
bash = named "Bash" . targeted . attacking 8 . debuffing 2 Vulnerable . card

bash' :: CardSpec
bash' = named "Bash+" . targeted . attacking 10 . debuffing 3 Vulnerable . card

spotWeakness :: CardSpec
spotWeakness = named "Spot Weakness" . targeted . withAction act . card
  where
  act tgt = do EnemyTurn is _ <- get (enemy tgt ~> enemyAI)
               case lookup WillAttack is of
                 Just _ -> gainBuff player 3 Strength
                 _ -> pure ()


seek :: CardSpec
seek = named "Seek" . exhausting . withAction seekAct . card
  where
  seekAct _ =
    do d  <- viewDrawPile
       cs <- choose "Choose a card to add to the hand" (Exactly 1) d
       case cs of
         c : _ -> do removeCardFrom theDrawPile c
                     drawCard c
         [] -> pure ()

