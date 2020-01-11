module Enemies where

import Field
import State
import Game

dummy :: Entity -> EnemyState
dummy e = EnemyState
  { _enemyEnt = EntityState
                  { _entityId = e
                  , _entityName = "Dummy"
                  , _entityAttrs = setField (attribute Health) 15 noAttributes
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
                  , _entityAttrs   = setField (attribute Health) 15 noAttributes
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
       haveMinion <- isAlive m
       pure (if haveMinion then damagePlayer m else spawn)


