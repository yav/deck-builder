module Battle where

import Control.Monad
import Data.Map(Map)
import qualified Data.Map as Map

import Field
import Player
import Deck
import Enemy
import RNG

type EnemyId = Int

data Target = TargetPlayer | TargetEnemy !EnemyId


data Battle = Battle
  { player        :: Player
  , deck          :: Deck Card
  , enemies       :: Map EnemyId Enemy
  , nextEnemyId   :: !EnemyId
  , hand          :: [Card]
  }

thePlayer :: FieldOf Battle Player
thePlayer b = (player b, \p -> b { player = p })

theDeck :: FieldOf Battle (Deck Card)
theDeck b = (deck b, \d -> b { deck = d })

theHand :: FieldOf Battle [Card]
theHand b = (hand b, \new -> b { hand = new })

theEnemies :: FieldOf Battle (Map Int Enemy)
theEnemies b = (enemies b, \es -> b { enemies = es })

theNextEnemyId :: FieldOf Battle EnemyId
theNextEnemyId b = (nextEnemyId b, \i -> b { nextEnemyId = i })


newtype BattleM a = BattleM { runBattleM :: Battle -> Either String (a,Battle) }

instance Functor BattleM where
  fmap = liftM

instance Applicative BattleM where
  pure a = BattleM (\b -> Right (a,b))
  (<*>)  = ap

instance Monad BattleM where
  BattleM m >>= k = BattleM (\b -> do (a,b1) <- m b
                                      let BattleM m1 = k a
                                      m1 b1)

getBattle :: BattleM Battle
getBattle = BattleM $ \b -> Right (b,b)

getField :: FieldOf Battle a -> BattleM a
getField f = BattleM (\b -> Right (get f b, b))

setField :: FieldOf Battle a -> a -> BattleM ()
setField f a = BattleM (\b -> Right ((), set f a b))

updField :: FieldOf Battle a -> (a -> a) -> BattleM ()
updField fi f = do x <- getField fi
                   setField fi (f x)

updField' :: FieldOf Battle a -> (a -> (b,a)) -> BattleM b
updField' fi f = do x <- getField fi
                    let (a,y) = f x
                    setField fi y
                    pure a


reportError :: String -> BattleM a
reportError err = BattleM (\_ -> Left err)

newEnemy :: Enemy -> BattleM ()
newEnemy e =
  do i <- getField theNextEnemyId
     setField theNextEnemyId (i + 1)
     updField theEnemies (Map.insert i e)


removeDeadEnemies :: BattleM ()
removeDeadEnemies = updField theEnemies (Map.filter notDead)
  where notDead e = get theEnemyHP e > 0


data Card = Card
  { cardName   :: String
  , cardAction :: Battle -> Target -> Maybe [Action]
  }

data Action = Action
  { actDescription :: String
  , actDo          :: BattleM ()
  }

instance Show Card where
  show = cardName
