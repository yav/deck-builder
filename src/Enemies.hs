module Enemies where

import Field
import State



basicEnemy :: Enemy -> EnemyState
basicEnemy e = EnemyState
                 { _enemyId = e
                 , _enemyName = "Dummy"
                 , _enemyAttrs = updateAttribute
                                   Health 15
                                   noAttributes
                 , _enemyAction = noActions -- actions 1
                 }
  where
  noActions = EnemyTurn (pure noActions)
  actions n = EnemyTurn $ do update (enemy e ~> enemyAttrs)
                                    (updateAttribute Health n)
                             pure (actions $! (n + 1))


boss :: Enemy -> EnemyState
boss e = EnemyState
  { _enemyId = e
  , _enemyName = "Boss"
  , _enemyAttrs = updateAttribute Health 15 noAttributes
  , _enemyAction  = actions Nothing
  }
  where
  actions mb =
    EnemyTurn
    case mb of
      Nothing -> do m <- newEnemy basicEnemy
                    pure (actions (Just m))
      Just m  -> do update (enemy m ~> enemyAttrs)
                           (updateAttribute Health (-5))
                    pure (actions mb)


