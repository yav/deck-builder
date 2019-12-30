module Cards where

import State
import Game



card :: Card -> CardEvents
card c = CardEvents
  { atEndOfTurn   = addTo theDiscarded c
  , afterPlay     = addTo theDiscarded c
  , whenPlay      = \_ -> pure ()
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

tageted :: CardEvents -> CardEvents
tageted ev = ev { isPlayable = \tgt -> case tgt of
                                         NoTaget -> pure False
                                         _       -> pure True }


noOp :: Int -> Card -> CardEvents
noOp i c = (card c)
  { cardName = "NoOp " ++ show i
  }

