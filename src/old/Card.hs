module Card where

import qualified Data.Map as Map

import Field
import Enemy
import Battle
import Player

strike :: Card
strike = Card
  { cardName = "Strike"
  , cardAction = \_ -> enemyAction $ \eid -> [damageEnemy eid 6]
  }

defend :: Card
defend = Card
  { cardName = "Defend"
  , cardAction = \_ -> playerAction [gainBlock 5]
  }

ironWave :: Card
ironWave = Card
  { cardName = "Iron Wave"
  , cardAction = \_ -> enemyAction $ \eid -> [ gainBlock 7
                                             , damageEnemy eid 7
                                             ]
  }


--------------------------------------------------------------------------------
damageEnemy :: EnemyId -> Integer -> Action
damageEnemy eid n = Action
  { actDescription = "Deal " ++ show n ++ " damage."
  , actDo = ifEnemy eid $ \e -> do let e' = upd theEnemyHP (subtract n) e
                                   updField theEnemies (Map.insert eid e')
  }

gainBlock :: Integer -> Action
gainBlock n = Action
  { actDescription = "Defend " ++ show n ++ "."
  , actDo          = updField thePlayer (updShield n)
  }

--------------------------------------------------------------------------------

ifEnemy :: EnemyId -> (Enemy -> BattleM ()) -> BattleM ()
ifEnemy eid k =
  do es <- getField theEnemies
     case Map.lookup eid es of
       Nothing -> pure ()
       Just e  -> k e

enemyAction :: (EnemyId -> [Action]) -> Target -> Maybe [Action]
enemyAction f t =
  case t of
    TargetPlayer  -> Nothing
    TargetEnemy n -> Just (f n)

playerAction :: [Action] -> Target -> Maybe [Action]
playerAction m t =
  case t of
    TargetPlayer  -> Just m
    TargetEnemy _ -> Nothing



