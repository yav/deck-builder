{-# Language RecordWildCards #-}
module Enemy where

import Field

data Enemy = Enemy
  { enemyHP :: !Integer
  } deriving Show


showEnemy :: Enemy -> String
showEnemy Enemy { .. } = "HP: " ++ show enemyHP

theEnemyHP :: FieldOf Enemy Integer
theEnemyHP e = (enemyHP e, \hp -> e { enemyHP = hp })


