module Enemies where

import Field
import State
import Game

basicEntity :: Entity -> EntityState
basicEntity e = EntityState
                 { _entityId = e
                 , _entityName = "Dummy"
                 , _entityAttrs = updateAttribute
                                   Health 15
                                   noAttributes
                 }

boss :: Entity -> EntityState
boss e = EntityState
  { _entityName    = "Boss"
  , _entityAttrs   = updateAttribute Health 15 noAttributes
  , _entityId = e
  }


multiAI :: [Action EnemyActions] -> Action EnemyActions
multiAI xs =
  do next <- sequence xs
     let un (EnemyTurn x) = x
     pure (EnemyTurn (multiAI (map un next)))



testAI :: Entity -> Action EnemyActions
testAI self = spawn
  where
  spawn =
    do set (intention self) "Spawn Minion"
       pure $ EnemyTurn $ do m <- newEntity basicEntity
                             multiAI [ minion m, damagePlayer m ]

  damagePlayer m =
    do set (intention self) "Attack 5"
       pure $ EnemyTurn
              do _ <- attack self player 5
                 h <- getAttribute Health <$> get (entity m ~> entityAttrs)
                 if h > 0 then damagePlayer m else spawn

  minion m =
    do set (intention m) "Gain block 3"
       pure $ EnemyTurn $
              do update (entity m ~> entityAttrs) (updateAttribute Block 3)
                 minion m

