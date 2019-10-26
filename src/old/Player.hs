{-# Language RecordWildCards #-}
module Player where

import Data.List(intercalate)

import Deck
import Field

data Player = Player
  { playerHP            :: Integer
  , playerShield        :: Integer
  , playerEnergyIncome  :: Integer
  , playerEnergy        :: Integer
  }

refillEnergy :: Player -> Player
refillEnergy p = p { playerEnergy = playerEnergy p + playerEnergyIncome p }



showPlayer :: Player -> String
showPlayer Player {..} = intercalate ", "
  [ "HP: " ++ show playerHP, "Block: " ++ show playerShield
  , "Energy: " ++ show playerEnergy ++ "/" ++ show playerEnergyIncome
  ]


theHP :: FieldOf Player Integer
theHP p = (playerHP p, \l -> p { playerHP = l })

theShield :: FieldOf Player Integer
theShield p = (playerShield p, \l -> p { playerShield = l })

theEnergy :: FieldOf Player Integer
theEnergy p = (playerEnergy p, \l -> p { playerEnergy = l })

-- | Either increase or decrease the HP of a player.
updHP :: Integer -> Player -> Player
updHP x = upd theHP (+ x)

-- | Either increase or decrease the shield of a player.
-- Of the shield goes negatve, it will be recet to 0, and
-- the player will loose the same amout of life.
updShield :: Integer -> Player -> Player
updShield x = adjHP . upd theShield (+ x)
  where
  adjHP p = let s = get theShield p
            in if s < 0 then set theShield 0 (updHP s p) else p



