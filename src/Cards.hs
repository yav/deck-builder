module Cards where

import Field
import State
import Game



card :: Card -> CardEvents
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
  , cardName      = "(no name)"
  }

ethereal :: CardEvents -> CardEvents
ethereal ev = ev { atEndOfTurn = exhaustCard (self ev) }

retain :: CardEvents -> CardEvents
retain ev = ev { atEndOfTurn = retainCard (self ev) }

power :: CardEvents -> CardEvents
power ev = ev { afterPlay = pure () }

targeted :: CardEvents -> CardEvents
targeted ev = ev { isPlayable = \tgt ->
  if tgt == player
      then pure False
      else do h <- getAttribute Health <$> get (entity tgt ~> entityAttrs)
              pure (h > 0)
  }

exhaustsOnPlay :: CardEvents -> CardEvents
exhaustsOnPlay ev = ev { afterPlay = exhaustCard (self ev) }

--------------------------------------------------------------------------------


noOp :: Int -> Card -> CardEvents
noOp i c = (card c)
  { cardName = "NoOp " ++ show i
  , whenPlay = \_ -> update (entity player ~> entityAttrs)
                            (updateAttribute Health 2)
  }


strike :: Card -> CardEvents
strike = custom . targeted . card
  where
  custom ca =
    ca { cardName = "Strike"
       , whenPlay = \tgt -> attack player tgt 6 >> pure ()
       }

defend :: Card -> CardEvents
defend = custom . card
  where
  custom ca =
    ca { cardName = "Defend"
       , whenPlay = \_ -> update (entity player ~> entityAttrs)
                                 (updateAttribute Block 5)
       }



seek :: Card -> CardEvents
seek = custom . exhaustsOnPlay . card
  where
  custom ca =
    ca { cardName = "Seek"
       , whenPlay = \_ ->
          do d  <- viewDrawPile
             cs <- choose "Choose a card to add to the hand" (Exactly 1) d
             case cs of
               c : _ -> do removeCardFrom theDrawPile c
                           cardDrawn c
               _ -> pure ()
       }


