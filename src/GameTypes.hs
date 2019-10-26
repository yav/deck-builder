module GameTypes where

data CharId = CharId Int
            | CardId Int
            | System
              deriving (Eq,Ord,Show)

data Event = Damage Integer
           | Attack Integer
           | Died
           | GainAttribute AttributeName Integer
           | ActivateCard CharId
           | Discard CardName CardLocation Integer
           | ShuffleDiscard
             deriving Show

data AttributeName = Count CardLocation
                   | Strength | Buffer | Thorns
                   | Block | HP
                   | Card CardName CardLocation
                   | Approved
  deriving (Eq,Ord, Show)  -- determines the order in which 

data CardName = Strike
  deriving (Eq,Ord,Show)

data CardLocation = InDraw | InHand | InDiscard | Exhausted
  deriving (Eq,Ord,Show)



