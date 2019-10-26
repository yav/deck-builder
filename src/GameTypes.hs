module GameTypes where

type CharId = Int

data Event = Damage Integer
           | GainBlock Integer
           | Attack Integer
           | Died
           | GainAttribute AttributeName Integer
           | ActivateCard CharId
             deriving Show

data AttributeName = Strength | Buffer | Thorns | HP
                   | Card CardName CardLocation
  deriving (Eq,Ord, Show)  -- determines the order in which 

data CardName = Strike
  deriving (Eq,Ord,Show)

data CardLocation = InDraw | InHand | InDiscard | Exhausted
  deriving (Eq,Ord,Show)

