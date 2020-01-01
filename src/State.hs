module State
  ( Action, Card, Entity

  -- * The State
  , doAction
  , State, newState
  , Script(..)

  -- * Fields
  , Field, get, set, update

  -- * Randomness
  , random, randomView

  -- * Statistics
  , turn

  -- * Piles
  , Pile
  , theDrawPile, theDiscarded, theExhausted, theHand
  , removeFrom, addTo, addToDrawBottom, addToDrawRandom
  , removeCardFrom


  -- * Interaction
  , choose, Count(..)

  -- * Events
  , CardEvents(..)
  , doEvent, doEvent1
  , newCard

  -- * Entities
  , player, entity, entityAttrs
  , EntityState(..)

  -- * Enemies
  , enemies
  , enemy
  , newEnemy
  , removeEnemy
  , EnemyState(..)
  , EnemyActions(..)
  , enemyAI

  -- * Attributes
  , Attribute(..)
  , module A

  -- * PP
  , ppCard

  ) where

import Data.List(delete)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(liftM,ap)
import RNG
import Field
import Attributes as A
import PP

newtype Card = Card Int
                  deriving (Show,Eq,Ord)

newtype Entity = Entity Int
                  deriving (Show,Eq,Ord)


player :: Entity
player = Entity 0



data State = State
  { _drawPile    :: [Card]
  , _hand        :: [Card]
  , _discarded   :: [Card]
  , _exhausted   :: [Card]

  , _rng         :: RNG
  , _viewRNG     :: RNG -- used for viewing stuff, does not affect
                        -- game state

  , _cardEvents  :: Map Card CardEvents

  , _player      :: EntityState

  , _enemyState  :: Map Entity EnemyState
  , _enemies     :: [Entity]

  , _nextCard    :: Card
  , _nextEntity  :: Entity

  , _turn        :: Int
  }


data EntityState = EntityState
  { _entityAttrs   :: Attributes
  , _entityName    :: String
  , _entityId      :: Entity
  }



data CardEvents = CardEvents
  { atEndOfTurn     :: Action ()
  , whenPlay        :: Entity -> Action ()
  , afterPlay       :: Action ()
  , whenDrawn       :: Action ()
  , whenExhausted   :: Action ()
  , whenRetained    :: Action ()
  , whenDiscarded   :: Action ()
  , isPlayable      :: Entity -> Action Bool
  , self            :: Card
  , cardName        :: String
  }




newState :: RNG -> State
newState r =
  withRNG r
  do viewRNG <- randRNG
     pure \newR ->
        State
          { _drawPile     = []
          , _hand         = []
          , _discarded    = []
          , _exhausted    = []
          , _rng          = newR
          , _viewRNG      = viewRNG
          , _cardEvents   = Map.empty
          , _nextCard     = Card 0
          , _nextEntity   = Entity 1
          , _player       = EntityState
                              { _entityAttrs  = noAttributes
                              , _entityName   = "Player"
                              , _entityId     = player
                              }
          , _enemyState   = Map.empty
          , _enemies      = []
          , _turn         = 0
          }

--------------------------------------------------------------------------------
newtype Action a = Action (forall r. (a -> Result r) -> Result r)
type Result r    = State -> Script r



choose :: String -> Count -> [Card] -> Action [Card]
choose msg c cs = Action \k -> case c of
                                 Exactly n
                                   | n >= length cs -> k cs
                                 _ -> \s -> Choose s msg c cs \sel -> k sel s

get :: Field a -> Action a
get f = Action \k -> \s -> k (getField f s) s

set :: Field a -> a -> Action ()
set f a = Action \k -> k () . setField f a

update :: Field a -> (a -> a) -> Action ()
update x f = set x . f =<< get x

doAction :: Action a -> State -> Script a
doAction (Action m) = m Done



instance Functor Action where
  fmap = liftM

instance Applicative Action where
  pure a = Action \k -> k a
  (<*>)  = ap

instance Monad Action where
  Action m >>= f = Action \k -> m \a -> let Action m1 = f a in m1 k


--------------------------------------------------------------------------------
-- Script

data Script a = Choose State String Count [Card] ([Card] -> Script a)
              | Done a State

data Count  = UpTo Int | Exactly Int
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

nextEntity :: Field Entity
nextEntity = Field { getField = _nextEntity
                   , setField = \x s -> s { _nextEntity = x }
                   }


entity :: Entity -> Field EntityState
entity e = if e == player
            then playerF
            else enemy e ~> enemyEntity

enemy :: Entity -> Field EnemyState
enemy e = enemiesF ~> mapField e

playerF :: Field EntityState
playerF = Field { getField = _player
                , setField = \x s -> s { _player = x }
                }

enemiesF :: Field (Map Entity EnemyState)
enemiesF = Field { getField = _enemyState
                 , setField = \x s -> s { _enemyState = x }
                 }

entityAttrs :: FieldOf EntityState Attributes
entityAttrs = Field { getField = _entityAttrs
                    , setField = \x s -> s { _entityAttrs = x }
                    }

turn :: Field Int
turn = Field { getField = _turn
             , setField = \x s -> s { _turn = x }
             }



data EnemyActions = EnemyTurn String (Action EnemyActions)

data EnemyState = EnemyState
  { _enemyEnt      :: EntityState
  , _enemyAI       :: EnemyActions
  }

enemyEntity :: FieldOf EnemyState EntityState
enemyEntity = Field { getField = _enemyEnt
                    , setField = \x s -> s { _enemyEnt = x }
                    }


enemyAI :: FieldOf EnemyState EnemyActions
enemyAI = Field { getField = _enemyAI
                , setField = \x s -> s { _enemyAI = x }
                }


enemies :: Field [Entity]
enemies = Field { getField = _enemies
                , setField = \x s -> s { _enemies = x }
                }

newEnemy :: (Entity -> EnemyState) -> Action Entity
newEnemy mk =
  do e@(Entity n) <- get nextEntity
     update enemiesF (Map.insert e (mk e))
     update enemies  (e :)
     set nextEntity (Entity (n + 1))
     pure e

removeEnemy :: Entity -> Action ()
removeEnemy e = update enemies (delete e)



--------------------------------------------------------------------------------

events :: Field (Map Card CardEvents)
events = Field { getField = _cardEvents
               , setField = \m s -> s { _cardEvents = m }
               }

doEvent1 :: Card -> (CardEvents -> p -> Action a) -> p -> Action a
doEvent1 c e p =
  do es <- get events
     e (es Map.! c) p

doEvent :: Card -> (CardEvents -> Action a) -> Action a
doEvent c e =
  do es <- get events
     e (es Map.! c)

newCard :: (Card -> CardEvents) -> Action Card
newCard es =
  do c@(Card n) <- get nextCard
     update events (Map.insert c (es c))
     set nextCard (Card (n + 1))
     pure c



--------------------------------------------------------------------------------

randomWith :: Field RNG -> Gen a -> Action a
randomWith rng m =
  do r0 <- get rng
     let (a,r) = withRNG r0 ((,) <$> m)
     set rng r
     pure a


-- | Do something with randomness.
random :: Gen a -> Action a
random = randomWith Field { getField = _rng
                          , setField = \a s -> s { _rng = a }
                          }

randomView :: Gen a -> Action a
randomView = randomWith Field { getField = _viewRNG
                              , setField = \a s -> s { _viewRNG = a }
                              }



-- | Remove the card at a specific location in a pile.
removeFrom :: Pile -> Int -> Action (Maybe Card)
removeFrom p n =
  do cs <- get p
     case removeAt n cs of
       Just (a,ys) -> set p ys *> pure (Just a)
       Nothing     -> pure Nothing

removeCardFrom :: Pile -> Card -> Action ()
removeCardFrom p c = update p (delete c)

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

instance PP State where
  pp s@State { .. } =
    vcat [ "=== Turn" <+> int _turn <+> "==="
         , "Enemies:"
         , nest 2 (numbered (map ppEnemy _enemies))
         , "Player:"
         , nest 2 (pp _player)
         , hsep [ "Piles:"
                , ppPile "draw" _drawPile
                , ppPile "discard" _discarded
                , ppPile "exhausted" _exhausted
                ]
        , "Hand:"
        , nest 2 (numbered (map (ppCard s) _hand))
        ]
    where
    ppPile x ys = x <.> colon <+> pp (length ys)
    ppEnemy e   = pp (getField (enemy e) s)

ppCard :: State -> Card -> Doc
ppCard s c = case Map.lookup c (_cardEvents s) of
               Just ca -> text (cardName ca)
               Nothing -> "?"

instance PP EntityState where
  pp EntityState { .. } =
    text _entityName $$ nest 2 (pp _entityAttrs)

instance PP EnemyState where
  pp EnemyState { .. } = pp _enemyEnt <+> brackets intent
    where intent = case _enemyAI of
                     EnemyTurn x _ -> text x


--------------------------------------------------------------------------------
insertAt :: Int -> a -> [a] -> [a]
insertAt n a xs = case splitAt n xs of
                    (front, back) -> front ++ a : back

removeAt :: Int -> [a] -> Maybe (a,[a])
removeAt n xs = case splitAt n xs of
                  (front, a : back) -> Just (a, front ++ back)
                  _                 -> Nothing


