module Cards where

import State
import Game



card :: Card -> CardEvents
card c = CardEvents
  { atEndOfTurn   = addTo theDiscarded c
  , afterPlay     = addTo theDiscarded c
  , whenPlay      = pure Unplayable
  , whenExhausted = pure ()
  , whenRetained  = pure ()
  , whenDiscarded = pure ()
  , self          = c
  }

ethereal :: CardEvents -> CardEvents
ethereal ev = ev { atEndOfTurn = exhaustCard (self ev) }

retain :: CardEvents -> CardEvents
retain ev = ev { atEndOfTurn = retainCard (self ev) }

power :: CardEvents -> CardEvents
power ev = ev { afterPlay = pure () }


