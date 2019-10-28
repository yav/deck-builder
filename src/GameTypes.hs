module GameTypes where

data CharId = CharId Int
            | CardId Int
            | System
              deriving (Eq,Ord,Show)

data Event = Damage Integer
           | Attack Integer
           | Died
           | AttributeName := Integer
           | ActivateCardOn CharId
           | MoveTo CardLocation
           | DrawTop  -- ^ draw top card from draw pile
           | DrawTopAgain -- ^ draw top after reshuffling
           | ShuffleDiscard ShuffleStatus
             deriving Show

data ShuffleStatus = ShuffleBegin
                   | ChooseOrder Integer
                   | ShuffleCards [Integer]
                    deriving Show

data AttributeName = Count CardLocation
                   | Strength | Buffer | Thorns
                   | Block | HP
                   | Card !CardInfo   -- value is index in deck
                   | Approved
  deriving (Eq,Ord, Show)  -- determines the order in which 

data CardInfo = CardInfo
  { cardName     :: CardName
  , cardLocation :: CardLocation
  } deriving (Eq,Ord,Show)

data CardName = Strike | Defend
  deriving (Eq,Ord,Show)

data CardLocation = InDraw | InHand | InDiscard | Exhausted
  deriving (Eq,Ord,Show)



