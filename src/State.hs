module State
  ( Action, Card

  -- * The State
  , doAction
  , State, newState
  , Script(..)

  -- * Fields
  , Field, get, set, update

  -- * Piles
  , Pile
  , theDrawPile, theDiscarded, theExhausted, theHand
  , removeFrom, addTo, addToDrawBottom, addToDrawRandom

  -- * Randomness
  , random

  -- * Interaction
  , choose, Count(..)

  -- * Events
  , CardEvents(..)
  , doEvent
  , newCard
  , PlayResult(..)

  -- * Entities
  , player, getEnemies, enemy, enemyAttrs
  , EnemyState

  -- * Attributes
  , Attribute(..)
  , module A

  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(liftM,ap)
import RNG
import Field
import Attributes as A

type Card = Int
type Enemy = Int

data State = State
  { _drawPile    :: [Card]
  , _hand        :: [Card]
  , _discarded   :: [Card]
  , _exhausted   :: [Card]
  , _rng         :: RNG
  , _cardEvents  :: Map Card CardEvents
  , _nextCard    :: Card
  , _player      :: Attributes
  , _enemies     :: Map Enemy EnemyState
  , _nextEnemy   :: Int
  }


data EnemyState = EnemyState
  { _enemyAttrs :: Attributes
  }

data PlayResult = Unplayable | Played | NeedsTarget

data CardEvents = CardEvents
  { atEndOfTurn     :: Action ()
  , whenPlay        :: Action PlayResult
  , afterPlay       :: Action ()
  , whenExhausted   :: Action ()
  , whenRetained    :: Action ()
  , whenDiscarded   :: Action ()
  , self            :: Card
  }

newState :: RNG -> State
newState r = State
  { _drawPile   = []
  , _hand       = []
  , _discarded  = []
  , _exhausted  = []
  , _rng        = r
  , _cardEvents = Map.empty
  , _nextCard   = 0
  , _nextEnemy  = 0
  , _player     = noAttributes
  , _enemies    = Map.empty
  }

--------------------------------------------------------------------------------
newtype Action a = Action ((a -> State -> Script) -> State -> Script)

choose :: String -> Count -> Int -> [Card] -> Action [Card]
choose msg c n cs = Action \k -> case c of
                                   Exactly
                                     | n >= length cs -> k cs
                                   _ -> \s -> Choose msg c n cs \sel -> k sel s

get :: Field a -> Action a
get f = Action \k -> \s -> k (getField f s) s

set :: Field a -> a -> Action ()
set f a = Action \k -> k () . setField f a

update :: Field a -> (a -> a) -> Action ()
update x f = set x . f =<< get x

doAction :: Action () -> State -> Script
doAction (Action m) = m \_ -> Done


instance Functor Action where
  fmap = liftM

instance Applicative Action where
  pure a = Action \k -> k a
  (<*>)  = ap

instance Monad Action where
  Action m >>= f = Action \k -> m \a -> let Action m1 = f a in m1 k


--------------------------------------------------------------------------------
-- Script

data Script = Choose String Count Int [Card] ([Card] -> Script)
            | Done State

data Count  = UpTo | Exactly
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Fields

type Field = FieldOf State
type Pile = Field [Card]

theDrawPile :: Pile
theDrawPile = Field { getField = _drawPile
                    , setField = \cs s -> s { _drawPile = cs }
                    }

theDiscarded :: Pile
theDiscarded = Field { getField = _discarded
                     , setField = \cs s -> s { _discarded = cs }
                     }


theExhausted :: Pile
theExhausted = Field { getField = _exhausted
                     , setField = \cs s -> s { _exhausted = cs }
                     }

theHand :: Pile
theHand = Field { getField = _hand
                , setField = \cs s -> s { _hand = cs }
                }


nextCard :: Field Card
nextCard = Field { getField = _nextCard
                 , setField = \x s -> s { _nextCard = x }
                 }

player :: Field Attributes
player = Field { getField = _player
               , setField = \x s -> s { _player = x }
               }

enemy :: Enemy -> Field EnemyState
enemy e = enemiesF ~> mapField e

enemiesF :: Field (Map Enemy EnemyState)
enemiesF = Field { getField = _enemies
                 , setField = \x s -> s { _enemies = x }
                 }

enemyAttrs :: FieldOf EnemyState Attributes
enemyAttrs = Field { getField = _enemyAttrs
                   , setField = \x s -> s { _enemyAttrs = x }
                   }

getEnemies :: Action [Enemy]
getEnemies = Map.keys <$> get enemiesF



--------------------------------------------------------------------------------

events :: Field (Map Card CardEvents)
events = Field { getField = _cardEvents
               , setField = \m s -> s { _cardEvents = m }
               }

doEvent :: Card -> (CardEvents -> Action a) -> Action a
doEvent c e =
  do es <- get events
     e (es Map.! c)

newCard :: (Card -> CardEvents) -> Action Card
newCard es =
  do c <- get nextCard
     update events (Map.insert c (es c))
     set nextCard (c + 1)
     pure c



--------------------------------------------------------------------------------

-- | Do something with randomness.
random :: Gen a -> Action a
random m =
  do r0 <- get rng
     let (a,r) = withRNG r0 ((,) <$> m)
     set rng r
     pure a
  where
  rng = Field { getField = _rng
              , setField = \a s -> s { _rng = a }
              }


-- | Remove the card at a specific location in a pile.
removeFrom :: Pile -> Int -> Action (Maybe Card)
removeFrom p n =
  do cs <- get p
     case removeAt n cs of
       Just (a,ys) -> set p ys *> pure (Just a)
       Nothing     -> pure Nothing

-- | Add a card to a gie pile.
-- Assumes the card is not in any other pile.
addTo :: Pile -> Card -> Action ()
addTo p c = update p (c :)

-- | Add a card to the bottom of the draw pile.
-- Assumes the card is not in any other pile.
addToDrawBottom :: Card -> Action ()
addToDrawBottom c = update theDrawPile (++ [c])


-- | Shuffle a card in the draw pile.
-- Assumes the card is not in any other pile.
addToDrawRandom :: Card -> Action ()
addToDrawRandom c =
  do cs <- get theDrawPile
     n  <- random (randInRange 0 (length cs + 1))
     set theDrawPile (insertAt n c cs)
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
insertAt :: Int -> a -> [a] -> [a]
insertAt n a xs = case splitAt n xs of
                    (front, back) -> front ++ a : back

removeAt :: Int -> [a] -> Maybe (a,[a])
removeAt n xs = case splitAt n xs of
                  (front, a : back) -> Just (a, front ++ back)
                  _                 -> Nothing


