module Enemies where

import Field
import State
import Game

dummy :: Entity -> EnemyState
dummy e = EnemyState
  { _enemyEnt = EntityState
                  { _entityId = e
                  , _entityName = "Dummy"
                  , _entityAttrs = updateAttribute Health 15 noAttributes
                  }
  , _enemyAI      = dummyAI 3
  }
  where
  dummyAI n = EnemyTurn [(WillAttack,0)]
              do d <- attack e player n
                 pure (dummyAI (n+d))



boss :: Entity -> EnemyState
boss e = EnemyState
  { _enemyEnt = EntityState
                  { _entityName    = "Boss"
                  , _entityAttrs   = updateAttribute Health 15 noAttributes
                  , _entityId      = e
                  }
  , _enemyAI = spawn
  }
  where
  spawn =
    EnemyTurn []
    do m <- newEnemy dummy
       pure (damagePlayer m)

  damagePlayer m =
    EnemyTurn [(WillAttack,0)]
    do _ <- attack e player 5
       h <- getAttribute Health <$> get (entity m ~> entityAttrs)
       pure (if h > 0 then damagePlayer m else spawn)


